module mycpu_top(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_en,   // chip select signal of instruction sram
    output wire [ 3:0] inst_sram_we,
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    // data sram interface
    output wire        data_sram_en,   // chip select signal of data sram
    output wire [ 3:0] data_sram_we,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire [31:0] data_sram_rdata,
    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata
);

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Declarations
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

reg         reset;
always @(posedge clk) reset <= ~resetn;

// reg         valid;
// always @(posedge clk) begin
//     if (reset) begin
//         valid <= 1'b0;
//     end
//     else begin
//         valid <= 1'b1;
//     end
// end

wire [31:0] seq_pc;
wire [31:0] nextpc;
wire        br_taken;
wire [31:0] br_target;
wire [31:0] inst;
reg  [31:0] pc;

wire [11:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        dst_is_r1;
wire        gr_we;
wire        mem_we;
wire        src_reg_is_rd;
wire [4: 0] dest;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] imm;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;
wire        inst_sub_w;
wire        inst_slt;
wire        inst_sltu;
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_w;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_lu12i_w;

// exp10: additional arithmetic and logic operation instructions
// Arithmetic and logical operation instructions
wire        inst_slti;
wire        inst_sltui;
wire        inst_andi;
wire        inst_ori;
wire        inst_xori;
wire        inst_sll_w;
wire        inst_srl_w;
wire        inst_sra_w;
wire        inst_pcaddu12i;

// Multiplication and division instructions
wire        inst_mul_w;
wire        inst_mulh_w;
wire        inst_mulh_wu;
wire        inst_div_w;
wire        inst_mod_w;
wire        inst_div_wu;
wire        inst_mod_wu;


wire        need_ui5;  // unsigned immediate 5 bit
wire        need_si12; // signed immediate 12 bit
wire        need_ui12; // exp10: unsigned immediate 12 bit
wire        need_si16; // signed immediate 16 bit
wire        need_si20; // signed immediate 20 bit
wire        need_si26; // signed immediate 26 bit
wire        src2_is_4;

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;

wire [31:0] alu_src1   ;
wire [31:0] alu_src2   ;
wire [31:0] alu_result ;

wire [31:0] mem_result;

wire [31:0] final_result; // debug: final_result未声明


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// pipeline signals
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [ 4:0] pipe_valid;  // IF ID EX MEM WB
wire [ 4:0] pipe_allowin;
wire [ 4:0] pipe_ready_go;
wire [ 3:0] pipe_tonext_valid;

assign pipe_allowin[ 3:0] = ~pipe_valid[ 3:0] | pipe_ready_go[ 3:0] & pipe_allowin[ 4:1];
assign pipe_allowin[4] = ~pipe_valid[4] | pipe_ready_go[4];
                                       
assign pipe_tonext_valid[ 3:0] = pipe_allowin[ 4:1] & pipe_ready_go[ 3:0];

// valid signal control in pipeline
always @(posedge clk) begin
    if (reset) begin
        pipe_valid <= 5'b00000;
    end
    else begin
        if (pipe_allowin[0]) begin
                pipe_valid[0] <= 1'b1;
        end
        if (br_taken) begin
            pipe_valid[1] <= 1'b0;
            if (pipe_tonext_valid[2]) begin
                pipe_valid[2] <= 1'b0;
            end
        end
        else  begin
            if (pipe_allowin[1]) begin
                pipe_valid[1] <= pipe_ready_go[0];
            end
            if (pipe_allowin[2]) begin
                pipe_valid[2] <= pipe_ready_go[1];
            end
        end
        if (pipe_allowin[3]) begin
            pipe_valid[3] <= pipe_ready_go[2];
        end
        if (pipe_allowin[4]) begin
            pipe_valid[4] <= pipe_ready_go[3];
        end
    end
end



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// IF stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


always @(posedge clk) begin
    if (reset) begin
        pc <= 32'h1bfffffc;     //trick: to make nextpc be 0x1c000000 during reset 
    end
    else if (br_taken) begin
        pc <= nextpc;
    end
    else if (pipe_allowin[0])begin
        pc <= nextpc;
    end
end

assign inst = inst_sram_rdata;  // instruction memory read data

// store the first instruction in the pipeline when IF stays more than 1 cycle

reg first_IF; // signal for the first instruction in the pipeline
reg [31:0] inst_IF_reg; // register for store the instruction

always @(posedge clk) begin
    if (reset) begin
        first_IF <= 1'b1;
    end
    else if (br_taken) begin
        first_IF <= 1'b1;
    end
    else if (pipe_tonext_valid[0]) begin
        first_IF <= 1'b1;
    end
    else begin
        first_IF <= !pipe_valid[0];
    end
end

always @(posedge clk) begin
    if (first_IF) begin
        inst_IF_reg <= inst;
    end
end

// pre-IF stage

assign seq_pc       = pc + 3'h4;
assign nextpc       = br_taken ? br_target : seq_pc;

assign inst_sram_en    = 1'b1;  // instruction memory enable
assign inst_sram_we    = 4'b0;  // instruction memory strb
assign inst_sram_addr  = nextpc;  // instruction memory address
assign inst_sram_wdata = 32'b0;  // instruction memory write data

assign pipe_ready_go[0] = pipe_valid[0];


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// IF stage to ID stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] inst_ID;
reg  [31:0] pc_ID;

always @(posedge clk) begin
    if (pipe_tonext_valid[0]) begin
        if (first_IF) begin
            inst_ID <= inst;
        end else begin
            inst_ID <= inst_IF_reg;
        end

        pc_ID   <= pc;
    end
end


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ID stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


assign op_31_26  = inst_ID[31:26];
assign op_25_22  = inst_ID[25:22];
assign op_21_20  = inst_ID[21:20];
assign op_19_15  = inst_ID[19:15];

assign rd   = inst_ID[ 4: 0];
assign rj   = inst_ID[ 9: 5];
assign rk   = inst_ID[14:10];

assign i12  = inst_ID[21:10];
assign i20  = inst_ID[24: 5];
assign i16  = inst_ID[25:10];
assign i26  = {inst_ID[ 9: 0], inst_ID[25:10]};

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));

assign inst_add_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];
assign inst_nor       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or        = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];
assign inst_slli_w    = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w    = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w    = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];
assign inst_addi_w    = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_ld_w      = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_w      = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_jirl      = op_31_26_d[6'h13];
assign inst_b         = op_31_26_d[6'h14];
assign inst_bl        = op_31_26_d[6'h15];
assign inst_beq       = op_31_26_d[6'h16];
assign inst_bne       = op_31_26_d[6'h17];
assign inst_lu12i_w   = op_31_26_d[6'h05] & ~inst_ID[25];

// exp10: additional arithmetic and logic operation instructions
// Arithmetic and logical operation instructions
assign inst_slti      = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui     = op_31_26_d[6'h00] & op_25_22_d[4'h9];
assign inst_andi      = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori       = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori      = op_31_26_d[6'h00] & op_25_22_d[4'hf];
assign inst_sll_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_srl_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_sra_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];
assign inst_pcaddu12i = op_31_26_d[6'h07] & ~inst_ID[25];

// Multiplication and division instructions
assign inst_mul_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_div_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_mod_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_div_wu    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mod_wu    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];


assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w | inst_jirl | inst_bl | inst_pcaddu12i;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w | inst_slti | inst_sltui;
assign need_ui12  =  inst_andi | inst_ori | inst_xori;
assign need_si16  =  inst_jirl | inst_beq | inst_bne;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_ui12 ? {20'b0, i12[11:0]}         :
/*need_ui5 || need_si12*/{{20{i12[11]}}, i12[11:0]} ;

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w;

assign src1_is_pc    = inst_jirl | inst_bl;

assign src2_is_imm   = inst_slli_w    | // shift instructions
                       inst_srli_w    |
                       inst_srai_w    |
                       inst_addi_w    | // alu instructions
                       inst_slti      |
                       inst_sltui     |
                       inst_andi      |
                       inst_ori       |
                       inst_xori      |
                       inst_ld_w      | // load instructions
                       inst_st_w      | // store instructions
                       inst_lu12i_w   | // load upper immediate instructions
                       inst_pcaddu12i |
                       inst_jirl      | // branch instructions
                       inst_bl        ;

assign res_from_mem  = inst_ld_w;
assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b;  // debug: gr_we wrong
assign mem_we        = inst_st_w;
assign dest          = dst_is_r1 ? 5'd1 : rd;

assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd :rk;
regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

// signals for blocking control
wire rf_using1;
wire rf_using2;

assign rf_using1 = inst_beq 
                || inst_bne 
                || inst_jirl // branch instructions without b/bl
                || inst_ld_w 
                || inst_st_w // load/store instructions
                || inst_add_w
                || inst_sub_w
                || inst_slt
                || inst_sltu
                || inst_nor
                || inst_and
                || inst_or
                || inst_xor // ALU without imm instructions
                || inst_sll_w
                || inst_srl_w
                || inst_sra_w // shift instructions without imm instructions
                || inst_slli_w
                || inst_srli_w
                || inst_srai_w
                || inst_slti
                || inst_sltui
                || inst_andi
                || inst_ori
                || inst_xori
                || inst_addi_w // imm instructions
;

assign rf_using2 = inst_beq 
                || inst_bne 
                || inst_jirl // branch instructions without b/bl
                // || inst_ld_w 
                // || inst_st_w // load/store instructions
                || inst_add_w
                || inst_sub_w
                || inst_slt
                || inst_sltu
                || inst_nor
                || inst_and
                || inst_or
                || inst_xor // ALU without imm instructions
                || inst_sll_w
                || inst_srl_w
                || inst_sra_w // shift instructions without imm instructions
                // || inst_slli_w
                // || inst_srli_w
                // || inst_srai_w
                // || inst_addi_w // imm instructions
;

assign rj_value  = df_alu_r1_EX  ? alu_result     :
                   df_alu_r1_MEM ? alu_result_MEM :
                   df_alu_r1_WB  ? alu_result_WB  :
                   df_ld_r1_MEM  ? mem_result     :
                   rf_rdata1
;

assign rkd_value = df_alu_r2_EX  ? alu_result     :
                   df_alu_r2_MEM ? alu_result_MEM :
                   df_alu_r2_WB  ? alu_result_WB  :
                   df_ld_r2_MEM  ? mem_result     :
                   rf_rdata2
;


// Pipeline blocking control
// signals for data forwarding

wire rd_eq_r1_EX;
wire rd_eq_r1_MEM;
wire rd_eq_r1_WB;
wire rd_eq_r2_EX;
wire rd_eq_r2_MEM;
wire rd_eq_r2_WB;

assign rd_eq_r1_EX  = (rf_raddr1 == dest_EX)  && (rf_raddr1 != 5'h0) && pipe_valid[2];
assign rd_eq_r1_MEM = (rf_raddr1 == dest_MEM) && (rf_raddr1 != 5'h0) && pipe_valid[3];
assign rd_eq_r1_WB  = (rf_raddr1 == dest_WB)  && (rf_raddr1 != 5'h0) && pipe_valid[4];
assign rd_eq_r2_EX  = (rf_raddr2 == dest_EX)  && (rf_raddr2 != 5'h0) && pipe_valid[2];
assign rd_eq_r2_MEM = (rf_raddr2 == dest_MEM) && (rf_raddr2 != 5'h0) && pipe_valid[3];
assign rd_eq_r2_WB  = (rf_raddr2 == dest_WB)  && (rf_raddr2 != 5'h0) && pipe_valid[4];

wire df_alu_r1_EX;
wire df_alu_r1_MEM;
wire df_alu_r1_WB;
wire df_alu_r2_EX;
wire df_alu_r2_MEM;
wire df_alu_r2_WB;
wire df_ld_r1_EX;
wire df_ld_r1_MEM;
wire df_ld_r1_WB;
wire df_ld_r2_EX;
wire df_ld_r2_MEM;
wire df_ld_r2_WB;

assign df_alu_r1_EX  = rd_eq_r1_EX  && gr_we_EX  && !inst_ld_w_EX;
assign df_alu_r1_MEM = rd_eq_r1_MEM && gr_we_MEM && !inst_ld_w_MEM;
assign df_alu_r1_WB  = rd_eq_r1_WB  && gr_we_WB  && !inst_ld_w_WB;
assign df_alu_r2_EX  = rd_eq_r2_EX  && gr_we_EX  && !inst_ld_w_EX;
assign df_alu_r2_MEM = rd_eq_r2_MEM && gr_we_MEM && !inst_ld_w_MEM;
assign df_alu_r2_WB  = rd_eq_r2_WB  && gr_we_WB  && !inst_ld_w_WB;
assign df_ld_r1_EX   = rd_eq_r1_EX  && inst_ld_w_EX;
assign df_ld_r1_MEM  = rd_eq_r1_MEM && inst_ld_w_MEM;
assign df_ld_r1_WB   = rd_eq_r1_WB  && inst_ld_w_WB;
assign df_ld_r2_EX   = rd_eq_r2_EX  && inst_ld_w_EX;
assign df_ld_r2_MEM  = rd_eq_r2_MEM && inst_ld_w_MEM;
assign df_ld_r2_WB   = rd_eq_r2_WB  && inst_ld_w_WB;

wire ID_stay;

assign ID_stay   = df_ld_r1_EX   && rf_using1
                || df_alu_r1_MEM && rf_using1
                || df_ld_r2_EX   && rf_using2
                || df_alu_r2_MEM && rf_using2
;

assign pipe_ready_go[1] = pipe_valid[1] && !ID_stay;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ID stage to EX stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] pc_EX;
reg  [11:0] alu_op_EX;
// inst type
// reg  inst_add_w_EX;
// reg  inst_sub_w_EX;
// reg  inst_slt_EX;
// reg  inst_sltu_EX;
// reg  inst_nor_EX;
// reg  inst_and_EX;
// reg  inst_or_EX;
// reg  inst_xor_EX;
// reg  inst_slli_w_EX;
// reg  inst_srli_w_EX;
// reg  inst_srai_w_EX;
// reg  inst_addi_w_EX;
reg  inst_ld_w_EX;
reg  inst_st_w_EX;
reg  inst_jirl_EX;
reg  inst_b_EX;
reg  inst_bl_EX;
reg  inst_beq_EX;
reg  inst_bne_EX;
// reg  inst_lu12i_w_EX;

reg  [31:0] imm_EX;
reg  [31:0] br_offs_EX;
reg  [31:0] jirl_offs_EX;
reg         src1_is_pc_EX;
reg         src2_is_imm_EX;

reg         res_from_mem_EX;
reg         gr_we_EX;
reg         mem_we_EX;
reg  [ 4:0] dest_EX;

reg  [31:0] rj_value_EX;
reg  [31:0] rkd_value_EX;

always @(posedge clk) begin
    pc_EX           <= pc_ID;
    alu_op_EX       <= alu_op;
//    inst_add_w_EX   <= inst_add_w;
//    inst_sub_w_EX   <= inst_sub_w;
//    inst_slt_EX     <= inst_slt;
//    inst_sltu_EX    <= inst_sltu;
//    inst_nor_EX     <= inst_nor;
//    inst_and_EX     <= inst_and;
//    inst_or_EX      <= inst_or;
//    inst_xor_EX     <= inst_xor;
//    inst_slli_w_EX  <= inst_slli_w;
//    inst_srli_w_EX  <= inst_srli_w;
//    inst_srai_w_EX  <= inst_srai_w;
//    inst_addi_w_EX  <= inst_addi_w;
    inst_ld_w_EX    <= inst_ld_w;
    inst_st_w_EX    <= inst_st_w;
    inst_jirl_EX    <= inst_jirl;
    inst_b_EX       <= inst_b;
    inst_bl_EX      <= inst_bl;
    inst_beq_EX     <= inst_beq;
    inst_bne_EX     <= inst_bne;
//    inst_lu12i_w_EX <= inst_lu12i_w;
    imm_EX          <= imm;
    br_offs_EX      <= br_offs;
    jirl_offs_EX    <= jirl_offs;
    src1_is_pc_EX   <= src1_is_pc;
    src2_is_imm_EX  <= src2_is_imm;
    res_from_mem_EX <= res_from_mem;
    gr_we_EX        <= gr_we;
    mem_we_EX       <= mem_we;
    dest_EX         <= dest;
    rj_value_EX     <= rj_value;
    rkd_value_EX    <= rkd_value;
end


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// EX stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

reg first_EX; // signal for the first instruction in the pipeline
always @(posedge clk) begin
    if(reset) begin
        first_EX <= 1'b1;
    end
    else if(pipe_tonext_valid[1]) begin
        first_EX <= 1'b1;
    end
    else begin
        first_EX <= 1'b0;
    end
end

assign rj_eq_rd = (rj_value_EX == rkd_value_EX);
assign br_taken = (   inst_beq_EX  &&  rj_eq_rd
                   || inst_bne_EX  && !rj_eq_rd
                   || inst_jirl_EX
                   || inst_bl_EX
                   || inst_b_EX
                  ) && pipe_valid[2] && first_EX;
assign br_target = (inst_beq_EX || inst_bne_EX || inst_bl_EX || inst_b_EX) ? (pc_EX + br_offs_EX) :
                                                   /*inst_jirl*/ (rj_value_EX + jirl_offs_EX);

assign alu_src1 = src1_is_pc_EX  ? pc_EX[31:0] : rj_value_EX;
assign alu_src2 = src2_is_imm_EX ? imm_EX : rkd_value_EX;

alu u_alu(
    .alu_op     (alu_op_EX ),
    .alu_src1   (alu_src1  ), // debug: 实例化ALU模块时，alu_src1 和 alu_src2 都连接到了 alu_src2 信号， 将 alu_src1 信号连接到 alu_src1 信号
    .alu_src2   (alu_src2  ),
    .alu_result (alu_result)
    );

assign data_sram_en    = 1'b1;
assign data_sram_we    = {4{mem_we_EX && pipe_valid[2]}};
assign data_sram_addr  = alu_result;
assign data_sram_wdata = rkd_value_EX;

assign pipe_ready_go[2] = pipe_valid[2];


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// EX stage to MEM stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] pc_MEM;
reg  [31:0] alu_result_MEM;

reg         inst_ld_w_MEM;
reg         inst_st_w_MEM;

reg         res_from_mem_MEM;
reg         gr_we_MEM;
reg  [ 4:0] dest_MEM;

always @(posedge clk) begin
    pc_MEM           <= pc_EX;
    alu_result_MEM   <= alu_result;
    res_from_mem_MEM <= res_from_mem_EX;
    gr_we_MEM        <= gr_we_EX;
    dest_MEM         <= dest_EX;  
    inst_ld_w_MEM    <= inst_ld_w_EX;
    inst_st_w_MEM    <= inst_st_w_EX;  
end


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MEM stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


assign mem_result   = data_sram_rdata;

assign pipe_ready_go[3] = pipe_valid[3];


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MEM stage to WB stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] pc_WB;
reg  [31:0] alu_result_WB;
reg  [31:0] mem_result_WB;

reg         inst_ld_w_WB;
reg         inst_st_w_WB;

reg         res_from_mem_WB;
reg         gr_we_WB;
reg  [ 4:0] dest_WB;

always @(posedge clk) begin
    if (reset) begin
        pc_WB           <= 32'h0;
//        alu_result_WB   <= 32'h0;
//        mem_result_WB   <= 32'h0;
        res_from_mem_WB <= 1'b0;
        gr_we_WB        <= 1'b0;
        dest_WB         <= 5'h0;
    end
    else begin
        pc_WB           <= pc_MEM;
        alu_result_WB   <= alu_result_MEM;
        mem_result_WB   <= mem_result;
        res_from_mem_WB <= res_from_mem_MEM;
        gr_we_WB        <= gr_we_MEM;
        dest_WB         <= dest_MEM;
        inst_ld_w_WB    <= inst_ld_w_MEM;
        inst_st_w_WB    <= inst_st_w_MEM;
    end
end


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// WB stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


assign final_result = res_from_mem_WB ? mem_result_WB : alu_result_WB;

assign rf_we    = gr_we_WB && pipe_valid[4];
assign rf_waddr = dest_WB;
assign rf_wdata = final_result;

assign pipe_ready_go[4] = pipe_valid[4];

// debug info generate
assign debug_wb_pc       = pc_WB;
assign debug_wb_rf_we    = {4{rf_we}};  // debug: 生成debug信息时，应为debug_wb_rf_we
assign debug_wb_rf_wnum  = dest_WB;
assign debug_wb_rf_wdata = final_result;

endmodule
