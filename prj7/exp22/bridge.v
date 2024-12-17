module bridge(
    // axi4-lite interface
    input  wire        aclk,
    input  wire        aresetn,
    // read request interface
    output wire [ 3:0] arid,
    output wire [31:0] araddr,
    output wire [ 7:0] arlen,
    output wire [ 2:0] arsize,
    output wire [ 1:0] arburst,
    output wire [ 1:0] arlock,
    output wire [ 3:0] arcache,
    output wire [ 2:0] arprot,
    output wire        arvalid,
    input  wire        arready,
    // read response interface
    input  wire [ 3:0] rid,
    input  wire [31:0] rdata,
    input  wire [ 1:0] rresp,
    input  wire        rlast,
    input  wire        rvalid,
    output wire        rready,
    // write request interface
    output wire [ 3:0] awid,
    output wire [31:0] awaddr,
    output wire [ 7:0] awlen,
    output wire [ 2:0] awsize,
    output wire [ 1:0] awburst,
    output wire [ 1:0] awlock,
    output wire [ 3:0] awcache,
    output wire [ 2:0] awprot,
    output wire        awvalid,
    input  wire        awready,
    // write data interface
    output reg  [ 3:0] wid,
    output wire [31:0] wdata,
    output reg  [ 3:0] wstrb,
    output wire        wlast,
    output wire        wvalid,
    input  wire        wready,
    // write response interface
    input  wire [ 3:0] bid,
    input  wire [ 1:0] bresp,
    input  wire        bvalid,
    output wire        bready,

    //SRAM interface
    // inst sram interface
    input  wire        inst_sram_req,   // chip select signal of instruction sram
    input  wire        inst_sram_wr,
    input  wire [ 1:0] inst_sram_size,
    input  wire [ 3:0] inst_sram_wstrb,
    input  wire [31:0] inst_sram_addr,
    input  wire [31:0] inst_sram_wdata,
    output wire [31:0] inst_sram_rdata,
    output wire        inst_sram_addr_ok,
    output wire        inst_sram_data_ok,
    input  wire [ 2:0] icache_rd_type,  // exp21
    // data sram interface
    input  wire        data_sram_req,   // chip select signal of data sram 
    input  wire        data_sram_wr,
    input  wire [ 1:0] data_sram_size,
    input  wire [ 3:0] data_sram_wstrb,
    input  wire [31:0] data_sram_addr,
    // input  wire [31:0] data_sram_wdata,
    output wire [31:0] data_sram_rdata,
    output wire        data_sram_addr_ok,
    output wire        data_sram_data_ok,
    input  wire        data_waddr_ok,
    input  wire        data_wdata_ok,
    input  wire        data_write_ok,
    input  wire        data_raddr_ok,
    input  wire        data_rdata_ok,
    input  wire        inst_raddr_ok,
    input  wire        memory_access,
    input  wire        inst_sram_using,
    input  wire [ 2:0] dcache_rd_type, // exp22
    input  wire [ 2:0] dcache_wr_type, // exp22
    input  wire [127:0]dcache_wr_data,// exp22
    input  wire        dcache_cachable,// exp22
    input  wire        dcache_write_refill// exp22
);

reg [31:0] wdata_buffer [3:0];
reg [7:0]  wlen;

assign arid = (!memory_access | (memory_access && (data_write_ok & ~(dcache_cachable & dcache_write_refill & (data_sram_wr & ~write_to_read)) | data_rdata_ok)) | inst_sram_using) ? 4'b0000 : 4'b0001; //0指令，1数据
assign araddr = (arid == 4'b0) ? inst_sram_addr : data_sram_addr; //读地址
assign arlen  = (arid == 4'b0) ? {2{icache_rd_type[2]}} : {2{dcache_rd_type[2]}}; //读长度
assign arsize = (arid == 4'b0) ? inst_sram_size : data_sram_size; //读大小
assign arburst = 2'b01; //固定为01
assign arlock = 2'b00; //固定为0
assign arcache = 4'b0000; //固定为0
assign arprot = 3'b000; //固定为0
assign arvalid = (inst_sram_req) | (reg_data_sram_req & ~(data_sram_wr & ~write_to_read)); //读请求有效

assign rready = (data_raddr_ok & !data_rdata_ok) | (inst_raddr_ok & (!memory_access || (memory_access && (data_write_ok & ~(dcache_cachable & dcache_write_refill & (data_sram_wr & ~write_to_read)) || data_rdata_ok))))
                | inst_sram_using & inst_raddr_ok; // 数据读完成握手或指令读完成握手

assign awid = 4'b0001; //固定为1
assign awaddr = data_sram_addr; //写地址
assign awlen = (reg_data_sram_req && (data_sram_wr & ~write_to_read)) ? {2{dcache_wr_type[2]}} : 8'b00000000; //固定为0
assign awsize = data_sram_size; //写大小
assign awburst = 2'b01; //固定为01
assign awlock = 2'b00; //固定为0
assign awcache = 4'b0000; //固定为0
assign awprot = 3'b000; //固定为0
assign awvalid = reg_data_sram_req & (data_sram_wr & ~write_to_read); //写请求有效

reg    reg_data_sram_req;

always @(posedge aclk) begin
    if(~aresetn) begin
        reg_data_sram_req <= 1'b0;
    end
    else if(awvalid && awready || arvalid && arready) begin
        reg_data_sram_req <= 1'b0;
    end
    else if(data_sram_req) begin
        reg_data_sram_req <= 1'b1;
    end
end

// assign wid = 4'b0001; //固定为1
// assign wstrb = data_sram_wstrb; //写掩码

always @(posedge aclk) begin
    if (~aresetn) begin
        {wdata_buffer[3],wdata_buffer[2],wdata_buffer[1],wdata_buffer[0]} <= 128'b0;
        wstrb <= 4'b0;
        wid   <= 4'b1;
    end
    else if(data_sram_req & (data_sram_wr & ~write_to_read)) begin
        {wdata_buffer[3],wdata_buffer[2],wdata_buffer[1],wdata_buffer[0]} <= dcache_wr_data;
        wstrb <= data_sram_wstrb;
    end
end

always @(posedge aclk) begin
    if(~aresetn) begin
        wlen <= 8'b0;
    end
    else if(data_sram_req & (data_sram_wr & ~write_to_read)) begin
        wlen <= {6'b0, {2{dcache_wr_type[2]}}};
    end
    else if(wvalid & wready) begin
        wlen <= wlen - 1;
    end
end

assign wlast = ~|wlen[1:0]; 
assign wdata = wdata_buffer[~wlen[1:0]]; //写数据
assign wvalid = data_waddr_ok & !data_wdata_ok; //写请求数据有效

assign bready = data_wdata_ok; // 写数据完成二次握手

assign inst_sram_rdata = rdata; //指令码
assign inst_sram_addr_ok = arvalid & arready & (arid == 4'b0); //指令地址有效
assign inst_sram_data_ok = rvalid & rready & inst_raddr_ok & rlast & (rid == 4'b0); //指令数据有效

assign data_sram_rdata = {32{arid == 4'b1}} & rdata; //读数据
assign data_sram_addr_ok = arvalid & arready & (arid == 4'b1) & ~(data_sram_wr & ~write_to_read) | awvalid & awready & (arid == 4'b1) & (data_sram_wr & ~write_to_read) & ~inst_sram_using; //数据地址有效
assign data_sram_data_ok = rvalid & rready & ~(data_sram_wr & ~write_to_read) & ((rlast & dcache_cachable) | ~dcache_cachable)
                         | bvalid & bready & (data_sram_wr & ~write_to_read) & ~inst_sram_using & ~(dcache_cachable & dcache_write_refill & (data_sram_wr & ~write_to_read))
                         ; //读写数据有效

reg write_to_read;

always @(posedge aclk) begin
    if(~aresetn) begin
        write_to_read <= 1'b0;
    end
    else if(bvalid & bready & (data_sram_wr & ~write_to_read) & ~inst_sram_using & dcache_cachable & dcache_write_refill) begin
        write_to_read <= 1'b1;
    end
    else if(data_sram_data_ok) begin
        write_to_read <= 1'b0;
    end
end

endmodule