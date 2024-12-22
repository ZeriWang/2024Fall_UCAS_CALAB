module cache (
    input          clk,
    input          resetn,
    // Coherent Cache / Strongly-ordered Uncached
    input          cachable,
    // interface to CPU
    input          valid,
    input          op, // 0: read, 1: write
    input  [ 7:0]  index,
    input  [19:0]  tag,
    input  [ 3:0]  offset,
    input  [ 3:0]  wstrb,
    input  [31:0]  wdata,
    output         addr_ok,
    output         data_ok,
    output [31:0]  rdata,
    output         cache_recv_addr,
    input          cacop,
    input  [ 4:0]  code,
    output         cache_write,
    // interface to axi
    output         rd_req,
    output [ 2:0]  rd_type, // 0: byte, 1: half, 2: word, 4: cache line
    output [31:0]  rd_addr,
    input          rd_rdy,
    input          ret_valid,
    input          ret_last,
    input  [31:0]  ret_data,
    output         wr_req,
    output [ 2:0]  wr_type, // 0: byte, 1: half, 2: word, 4: cache line
    output [31:0]  wr_addr,
    output [ 3:0]  wr_wstrb,
    output [127:0] wr_data,
    input          wr_rdy,
    input          data_write_ok,
    output         write_refill,
    output reg     write_complete
);

wire        tagv_we    [1:0];
wire [ 7:0] tagv_addr  [1:0];
wire [20:0] tagv_wdata [1:0];
wire [20:0] tagv_rdata [1:0];

reg  [255:0] dirty [1:0];

wire [ 3:0] data_bank_wstrb [1:0][3:0];
wire [ 7:0] data_bank_addr  [1:0][3:0];
wire [31:0] data_bank_wdata [1:0][3:0];
wire [31:0] data_bank_rdata [1:0][3:0];
wire        data_bank_we    [1:0][3:0];

wire        cache_write_new;

genvar i, j;

generate
    for (i = 0; i < 2; i = i + 1) begin
        TAGV_RAM u_tagv_ram (
            .clka   (clk            ),
            .wea    (tagv_we[i]     ),
            .addra  (tagv_addr[i]   ),
            .dina   (tagv_wdata[i]  ),
            .douta  (tagv_rdata[i]  ),
            .ena    (1'b1           )
        );
    end
endgenerate

generate
    for (i = 0; i < 2; i = i + 1) begin
        for (j = 0; j < 4; j = j + 1) begin
            DATA_BANK_RAM u_data_bank_ram (
                .clka   (clk                    ),
                .wea    (data_bank_wstrb[i][j]  ),
                .addra  (data_bank_addr[i][j]   ),
                .dina   (data_bank_wdata[i][j]  ),
                .douta  (data_bank_rdata[i][j]  ),
                // .ena    (data_bank_we[i][j]     )
                .ena    (1'b1)
            );
        end
    end
endgenerate

/*
    State Machine
*/
parameter IDLE    = 5'b00001;
parameter LOOKUP  = 5'b00010;
parameter MISS    = 5'b00100;
parameter REPLACE = 5'b01000;
parameter REFILL  = 5'b10000;

parameter WBUF_IDLE  = 2'b01;
parameter WBUF_WRITE = 2'b10;

reg [4:0] current_state, next_state;
reg [1:0] wbuf_current_state, wbuf_next_state;

always @(posedge clk) begin
    if (!resetn) begin
        current_state <= IDLE;
        wbuf_current_state <= WBUF_IDLE;
    end else begin
        current_state <= next_state;
        wbuf_current_state <= wbuf_next_state;
    end
end

// Main State Transition
always @(*) begin
    case (current_state)
        IDLE: begin
            if (valid && !hit_write_hazard) begin
                next_state = LOOKUP;
            end
            else if (!valid || valid && hit_write_hazard) begin
                next_state = IDLE;
            end
        end
        LOOKUP: begin
            if (cache_hit & (!valid | valid & hit_write_hazard) & ~cache_write) begin
                next_state = IDLE;
            end
            else if (cache_hit & valid & !hit_write_hazard & ~cache_write) begin
                next_state = LOOKUP;
            end
            else if (~reg_op && (!dirty[replace_way][reg_index] || !tagv_rdata[replace_way][0]) && reg_cachable && ~cacop) begin
                next_state = REPLACE;
            end
            else if (!cache_hit | cache_write) begin
                next_state = MISS;
            end
        end
        MISS: begin
            if (!wr_rdy) begin
                next_state = MISS;
            end
            else begin
                next_state = REPLACE;
            end
        end
        REPLACE: begin
            if(cache_write) begin
                next_state = IDLE;
            end
            else if (!rd_rdy) begin
                next_state = REPLACE;
            end
            else begin
                next_state = REFILL;
            end
        end
        REFILL: begin
            if ((ret_valid && ret_last) || ~reg_cachable) begin
                next_state = IDLE;
            end
            else begin
                next_state = REFILL;
            end
        end
        default: begin
            next_state = IDLE;
        end
    endcase
end

// Write Buffer State Transition
always @(*) begin
    case (wbuf_current_state)
        WBUF_IDLE: begin
            if (current_state == LOOKUP & hit_write & ~(cacop & code[0])) begin
                wbuf_next_state = WBUF_WRITE;
            end
            else begin
                wbuf_next_state = WBUF_IDLE;
            end
        end
        WBUF_WRITE: begin
            wbuf_next_state = WBUF_IDLE;
        end
        default: begin
            wbuf_next_state = WBUF_IDLE;
        end
    endcase
end

// Request Buffer
reg        reg_cachable;
reg        reg_op;
reg [ 7:0] reg_index;
reg [19:0] reg_tag;
reg [ 3:0] reg_offset;
reg [ 3:0] reg_wstrb;
reg [31:0] reg_wdata;

always @(posedge clk) begin
    if (!resetn) begin
        reg_cachable <= 1'b0;
        reg_op        <= 1'b0;
        reg_index     <= 8'b0;
        reg_tag       <= 20'b0;
        reg_offset    <= 4'b0;
        reg_wstrb     <= 4'b0;
        reg_wdata     <= 32'b0;
    end else begin
        if (next_state == LOOKUP) begin
            reg_cachable <= cachable;
            reg_op        <= op;
            reg_index     <= index;
            reg_tag       <= tag;
            reg_offset    <= offset;
            reg_wstrb     <= wstrb;
            reg_wdata     <= wdata;
        end
    end
end

reg [20:0] reg_tagv_dcacop;
always @(posedge clk) begin
    if (!resetn) begin
        reg_tagv_dcacop <= 20'b0;
    end
    else if (~(|reg_tagv_dcacop) & cacop & code[4:3] == 2'b01) begin
        reg_tagv_dcacop <= tagv_rdata[offset[0]];
    end
    else if (~(|reg_tagv_dcacop) & cacop & code[4:3] == 2'b10 & way0_hit) begin
        reg_tagv_dcacop <= tagv_rdata[0];
    end
    else if (~(|reg_tagv_dcacop) & cacop & code[4:3] == 2'b10 & way1_hit) begin
        reg_tagv_dcacop <= tagv_rdata[1];
    end
    else if((|reg_tagv_dcacop) & ~cacop) begin
        reg_tagv_dcacop <= 20'b0;
    end
end

// Tag Compare
wire way0_hit, way1_hit, cache_hit;
wire hit_write, hit_write_hazard;

assign way0_hit = tagv_rdata[0][0] & (tagv_rdata[0][20:1] == reg_tag) & reg_cachable;
assign way1_hit = tagv_rdata[1][0] & (tagv_rdata[1][20:1] == reg_tag) & reg_cachable;
assign cache_hit = (way0_hit | way1_hit) & ~(cacop & code[4:3] == 2'b01);
assign hit_write = cache_hit & reg_op;
assign hit_write_hazard = valid & !op & (current_state == LOOKUP & hit_write & index == reg_index & offset[3:2] == reg_offset[3:2] 
                                        || wbuf_current_state == WBUF_WRITE & offset[3:2] == reg_offset[3:2]);
assign cache_recv_addr = current_state == LOOKUP;

reg dcacop_hit0, dcacop_hit1;
always @(posedge clk) begin
    if (!resetn) begin
        dcacop_hit0 <= 1'b0;
        dcacop_hit1 <= 1'b0;
    end
    else if (cacop & code[4:3] == 2'b10 & way0_hit) begin
        dcacop_hit0 <= 1'b1;
    end
    else if (cacop & code[4:3] == 2'b10 & way1_hit) begin
        dcacop_hit1 <= 1'b1;
    end
    else if (~cacop) begin
        dcacop_hit0 <= 1'b0;
        dcacop_hit1 <= 1'b0;
    end
end

// Data Select
wire [ 31:0] way0_load_word, way1_load_word;
wire [ 31:0] load_res;

assign way0_load_word = data_bank_rdata[0][reg_offset[3:2]];
assign way1_load_word = data_bank_rdata[1][reg_offset[3:2]];
assign load_res = {32{ way0_hit}} & way0_load_word
                | {32{ way1_hit}} & way1_load_word
                | {32{ret_valid}} & ret_data;

// Miss Buffer & LFSR
reg          replace_way;
reg  [  1:0] ret_data_num;
wire [127:0] replace_data;
reg  [  2:0] lfsr;

always @(posedge clk) begin
    if (!resetn)
        lfsr <= 3'b001;
    else
        lfsr <= {lfsr[1:0], lfsr[2] ^ lfsr[1]};
end

always @(posedge clk) begin
    if (!resetn)
        replace_way <= 1'b0;
    else if (ret_valid && ret_last)
        replace_way <= lfsr[0];
end

always @(posedge clk) begin
    if (!resetn)
        ret_data_num <= 2'b00;
    else if (ret_valid && !ret_last)
        ret_data_num <= ret_data_num + 1;
    else if (ret_valid && ret_last)
        ret_data_num <= 2'b00;
end

assign replace_data = cacop & code[4:3] == 2'b01 ? {data_bank_rdata[offset[0]][3], data_bank_rdata[offset[0]][2], 
                      data_bank_rdata[offset[0]][1], data_bank_rdata[offset[0]][0]} :
                      cacop & code[4:3] == 2'b10 & dcacop_hit0 ? {data_bank_rdata[0][3], data_bank_rdata[0][2],
                      data_bank_rdata[0][1], data_bank_rdata[0][0]} :
                      cacop & code[4:3] == 2'b10 & dcacop_hit1 ? {data_bank_rdata[1][3], data_bank_rdata[1][2],
                      data_bank_rdata[1][1], data_bank_rdata[1][0]} :
                      {data_bank_rdata[replace_way][3], data_bank_rdata[replace_way][2], 
                      data_bank_rdata[replace_way][1], data_bank_rdata[replace_way][0]};

// Write Buffer
reg        hitwr_way;
reg [ 1:0] hitwr_bank;
reg [ 7:0] hitwr_index;
reg [ 3:0] hitwr_wstrb;
reg [31:0] hitwr_wdata;

always @(posedge clk) begin
    if (!resetn) begin
        hitwr_way     <= 1'b0;
        hitwr_bank <= 2'b00;
        hitwr_index <= 8'b0;
        hitwr_wstrb <= 4'b0;
        hitwr_wdata <= 32'b0;
    end
    else if (current_state == LOOKUP && hit_write) begin
        hitwr_way   <= way0_hit ? 1'b0 : 1'b1;
        hitwr_bank  <= reg_offset[3:2];
        hitwr_index <= reg_index;
        hitwr_wstrb <= reg_wstrb;
        hitwr_wdata <= reg_wdata;
    end
end

// TAGV
generate
    for (i = 0; i < 2; i = i + 1) begin
        assign tagv_we[i] = ret_valid & ret_last & replace_way == i & reg_cachable | cacop & offset[0] == i & code[4:3] != 2'b10 |
                            cacop_wr_tagv & tagv_rdata[i][0] & (tagv_rdata[i][20:1] == tag) & reg_cachable & code[4:3] == 2'b10;
        assign tagv_addr[i] = current_state == IDLE ? index : reg_index;
        assign tagv_wdata[i] = {reg_tag, 1'b1} & {21{ret_valid & ret_last & replace_way == i & reg_cachable}}|
                              {reg_tagv_dcacop[20:1], 1'b0} & {21{cacop & offset[0] == i & (code[4:3] == 2'b01 | code[4:3] == 2'b10)}}|
                              {21'b0} & {21{cacop & offset[0] == i & code[4:3] == 2'b00}};
    end
endgenerate

reg cacop_next;
always @(posedge clk) begin
    if (!resetn) begin
        cacop_next <= 1'b0;
    end
    else
        cacop_next <= cacop;
end

reg cacop_wr_tagv;
always @(posedge clk) begin
    if (!resetn) begin
        cacop_wr_tagv <= 1'b0;
    end
    else if (cacop & ~cacop_next) begin
        cacop_wr_tagv <= 1'b1;
    end
    else begin
        cacop_wr_tagv <= 1'b0;
    end
end

// DATA_BANK
generate
    for (i = 0; i < 2; i = i + 1) begin
        for (j = 0; j < 4; j = j + 1) begin
            assign data_bank_wstrb[i][j] = {4{data_bank_we[i][j]}} & {
                wbuf_current_state == WBUF_WRITE ?          hitwr_wstrb : 
                reg_op && reg_offset[3:2] == ret_data_num ? reg_wstrb : 
                                                            4'b1111
            };
            assign data_bank_addr[i][j]  = current_state == IDLE || current_state == LOOKUP ? index : reg_index;
            assign data_bank_wdata[i][j] = wbuf_current_state == WBUF_WRITE ?          hitwr_wdata : 
                                           reg_op && reg_offset[3:2] == ret_data_num ? reg_wdata :
                                                                                       ret_data;
            assign data_bank_we[i][j]    = wbuf_current_state == WBUF_WRITE ?          hitwr_way == i && hitwr_bank == j : 
                                                                                       replace_way == i && ret_data_num == j && ret_valid;
        end
    end
endgenerate

// Dirty
always @(posedge clk) begin
    if (!resetn) begin
        dirty[0] <= 256'b0;
        dirty[1] <= 256'b0;
    end
    else if (wbuf_current_state == WBUF_WRITE) begin
        dirty[hitwr_way][hitwr_index] <= 1'b1;
    end
    else if (ret_valid && ret_last && reg_cachable) begin
        dirty[replace_way][reg_index] <= reg_op;
    end
    else begin
        dirty[0] <= dirty[0];
        dirty[1] <= dirty[1];
    end
end


// Pipeline Interface
assign addr_ok = current_state == IDLE | current_state == LOOKUP & valid & cache_hit & (op | !op & !hit_write_hazard);
assign data_ok = (current_state == LOOKUP & cache_hit & ~cache_write) 
                | (current_state == LOOKUP & reg_op & ~(hit_write_hazard | (current_state == LOOKUP & next_state == MISS))) 
                | (current_state == REFILL & ret_valid & !reg_op & ((ret_data_num == reg_offset[3:2] & reg_cachable) | !reg_cachable));

assign rdata = load_res;

// AXI Interface
reg reg_wr_req;

always @(posedge clk) begin
    if(~resetn) begin
        write_complete <= 1'b0;
    end
    else if(data_write_ok || (reg_cachable && reg_op && ~(tagv_rdata[replace_way][0] && dirty[replace_way][reg_index]))) begin
        write_complete <= 1'b1;
    end
    else if(rd_req) begin
        write_complete <= 1'b0;
    end
end

assign rd_req = (current_state == REPLACE & (~reg_op | (reg_op & reg_cachable & write_complete))) & ~rd_rdy & ~(~reg_cachable & reg_op) & ~cache_write;
assign rd_type = reg_cachable ? 3'b100 : 3'b010;
assign rd_addr = {reg_tag, reg_index, ((~reg_cachable & ~reg_op) ? reg_offset : 4'b0)};

wire [ 7: 0] addr;
assign addr = ({8{(current_state[0] || current_state[1])}} & index) | ({8{(current_state[2] || current_state[3] || current_state[4])}} & reg_index);

always @(posedge clk) begin
    if (!resetn) begin
        reg_wr_req <= 1'b0;
    end
    else if (current_state == MISS & next_state == REPLACE & (tagv_rdata[replace_way][0] & dirty[replace_way][reg_index] | ~reg_cachable | 
            cacop & code[4:3] == 2'b01 & dirty[reg_offset[0]][reg_index] & reg_tagv_dcacop[0] |
            cacop & code[4:3] == 2'b10 & cache_write)) begin
        reg_wr_req <= 1'b1;
    end
    else if (wr_rdy) begin
        reg_wr_req <= 1'b0;
    end
end

assign cache_write_new = dirty[offset[0]][index] & tagv_rdata[offset[0]][0] & cacop & code[4:3] == 2'b01|
                         dirty[0][index] & tagv_rdata[0][0] & cacop & code[4:3] == 2'b10 & way0_hit |
                         dirty[1][index] & tagv_rdata[1][0] & cacop & code[4:3] == 2'b10 & way1_hit;
reg cache_write_reg;
always @(posedge clk) begin
    if (!resetn) begin
        cache_write_reg <= 1'b0;
    end
    else if (cache_write_new) begin
        cache_write_reg <= 1'b1;
    end
    else if (~cacop) begin
        cache_write_reg <= 1'b0;
    end
end

reg cacop_hit;
always @(posedge clk) begin
    if (!resetn) begin
        cacop_hit <= 1'b0;
    end
    else if (cacop & cache_write) begin
        cacop_hit <= 1'b1;
    end
    else if (~cacop) begin
        cacop_hit <= 1'b0;
    end
end

assign cache_write = (cache_write_reg | cache_write_new) & cacop & (code[4:3] == 2'b01 | code[4:3] == 2'b10 & (cacop_hit | cache_hit));

wire write_refill;

assign write_refill = current_state == REPLACE || current_state == REFILL;


assign wr_req = reg_wr_req;

assign wr_type = reg_cachable | cacop ? 3'b100 : 3'b010;
assign wr_wstrb = reg_cachable | cacop ? 4'b1111 : reg_wstrb;
assign wr_addr = reg_cachable & ~cacop ? {tagv_rdata[replace_way][20:1], reg_index, reg_offset} : 
                 cacop & code[4:3] == 2'b01 ? {reg_tagv_dcacop[20:1], reg_index, reg_offset[3:1], 1'b0} :
                 {reg_tag, reg_index, reg_offset};
assign wr_data = reg_cachable | cacop ? {8'hff, replace_data[119:0]} : {4{reg_wdata}};
endmodule