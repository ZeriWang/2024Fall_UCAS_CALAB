module mycpu_top(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_req,   // chip select signal of instruction sram
    output wire        inst_sram_wr,
    output wire [ 1:0] inst_sram_size,
    output wire [ 3:0] inst_sram_wstrb,
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    input  wire        inst_sram_addr_ok,
    input  wire        inst_sram_data_ok,
    // data sram interface
    output wire        data_sram_req,   // chip select signal of data sram
    output wire        data_sram_wr,
    output wire [ 1:0] data_sram_size,
    output wire [ 3:0] data_sram_wstrb,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire [31:0] data_sram_rdata,
    input  wire        data_sram_addr_ok,
    input  wire        data_sram_data_ok,
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

reg [63:0] cnt; // New cnt to read when rdcntvl.w/rdcntvh.w
always @(posedge clk) begin
    if (reset) begin
        cnt <= 64'h0;
    end
    else begin
        cnt <= cnt + 1;
    end
end

reg  flush_rst; // Flush reset signal, to ensure the flush signal only last one cycle
always @(posedge clk) begin
    if (reset) begin
        flush_rst <= 1'b1;
    end
    else if (flush) begin
        flush_rst <= 1'b0;
    end
    else if (~ex_WB && ~has_int_WB) begin
        flush_rst <= 1'b1;
    end
end
wire flush = (ex_WB || has_int_WB) && flush_rst; // Flush signal to flush the pipeline when exception or interrupt. The way to flush is to set the gr_we/csr_we/mem_we to 0 in all stages.

wire [31:0] seq_pc;
wire [31:0] nextpc;
wire        br_taken;
reg         br_taken_next[1:0];
wire [31:0] br_target;
wire [31:0] inst;
reg  [31:0] pc;

// exp10: mul & div
wire [ 2:0] mul_op;

wire [ 3:0] div_op;
wire        s_divisor_ready;
wire        s_divisor_valid;
wire        s_dividend_ready;
wire        s_dividend_valid;
wire        s_div_out_valid;
wire        u_divisor_ready;
wire        u_divisor_valid;
wire        u_dividend_ready;
wire        u_dividend_valid;
wire        u_div_out_valid;

wire [11:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        res_from_csr;
wire        dst_is_r1;
wire        gr_we;
wire        mem_we;
wire        src_reg_is_rd;
wire        rj_eq_rd;
wire        rj_l_rd;
wire        rj_lu_rd;
wire        rj_geq_rd;
wire        rj_gequ_rd;
wire [ 4:0] dest;
wire [13:0] csr_dest;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] imm;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_25_24;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] op_9_5;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [13:0] i14;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_25_24_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;
wire [31:0] op_9_5_d;

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
wire        inst_ld_b;
wire        inst_ld_h;
wire        inst_ld_bu;
wire        inst_ld_hu;
wire        inst_st_w;
wire        inst_st_b;
wire        inst_st_h; // exp11: add load/store instructions
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_blt;
wire        inst_bge;
wire        inst_bltu;
wire        inst_bgeu; // exp11: add branch instructions
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
wire        mul_inst;
wire        div_inst;

//csr instructions
wire        inst_csrrd;
wire        inst_csrwr;
wire        inst_csrxchg;
wire        inst_ertn;
wire        inst_syscall;
wire        inst_break;
wire        inst_rdcntid_w;
wire        inst_rdcntvl_w;
wire        inst_rdcntvh_w;

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
wire [31:0] alu_result_original;

// exp10: mul & div
wire [31:0] mul_src1   ;
wire [31:0] mul_src2   ;
wire [31:0] mul_result ;

wire [31:0] div_src1   ;
wire [31:0] div_src2   ;
wire [31:0] div_result ;
wire [63:0] s_div_out  ;
wire [63:0] u_div_out  ;

wire [31:0] mem_result;

wire [31:0] cnt_result; // Result from cnt

wire [31:0] final_result; // debug: final_result not declared

//exp12: csr
wire [13:0] csr_raddr;
wire [31:0] csr_rdata;
wire        csr_we;
wire        csr_we_real;
wire [31:0] csr_wmask;
wire [13:0] csr_waddr;
wire [31:0] csr_wdata;
wire [31:0] csr_value;
wire        csr_ertn;
wire        csr_wbex;
reg         csr_wbex_rst;
wire [ 5:0] csr_ecode;

wire [ 7:0] hw_int_in = 8'b0;
wire        ipi_int_in = 1'b0;
wire [31:0] coreid_in = 32'b0;
wire [31:0] ex_entry;

wire pc_unalign;
wire inst_not_exist;
wire addr_unalign;
wire ex_IF;
wire has_int;

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
        if (br_taken) begin
            pipe_valid[0] <= 1'b0;
        end
        else if (pipe_allowin[0]) begin
            pipe_valid[0] <= pipe_ready_go_preIF;
        end
        if (br_taken) begin
            pipe_valid[1] <= 1'b0;
            if (pipe_tonext_valid[2] && ~has_int) begin
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
    br_taken_next[0] <= br_taken;
    br_taken_next[1] <= br_taken_next[0];
end

always @(posedge clk) begin
    if (reset) begin
        pc <= 32'h1bfffffc;     //trick: to make nextpc be 0x1c000000 during reset 
    end
    else if (pipe_ready_go_preIF)begin
        pc <= nextpc;
    end
end

assign pc_unalign = pc[1:0] != 2'b00;
assign ex_IF = pc_unalign;
assign csr_ecode = pc_unalign ? 6'h8 : 6'h0;

// store the first instruction in the pipeline when IF stays more than 1 cycle

reg first_IF; // signal for the first instruction in the pipeline
reg [31:0] inst_IF_reg; // register for store the instruction
reg inst_IF_reg_valid; // signal for the valid of the instruction in the register
reg cancel_next_inst; // signal for cancel the next instruction

always @(posedge clk) begin
    if (reset) begin
        first_IF <= 1'b0;
    end
    else if (pipe_ready_go_preIF) begin
        first_IF <= 1'b1;
    end
    else if (inst_sram_data_ok) begin
        first_IF <= 1'b0;
    end
end

always @(posedge clk) begin
    if (reset) begin
        inst_IF_reg <= 32'h0;
    end
    else if (inst_sram_data_ok) begin
        inst_IF_reg <= inst_sram_rdata;
    end
end

always @(posedge clk) begin
    if (reset) begin
        inst_IF_reg_valid <= 1'b0;
    end
    else if (flush && !pipe_allowin[0] && pipe_ready_go[0]) begin
        inst_IF_reg_valid <= 1'b0;
    end
    else if (pipe_ready_go[0] && !pipe_allowin[1])
        inst_IF_reg_valid <= 1'b1;
    else 
        inst_IF_reg_valid <= 1'b0;
end

assign inst = first_IF || inst_sram_data_ok ? inst_sram_rdata : inst_IF_reg;

// pre-IF stage
wire pipe_ready_go_preIF;
reg [31:0] br_target_reg;
reg  br_taken_valid;

assign pipe_ready_go_preIF = inst_sram_req && inst_sram_addr_ok;    

always @(posedge clk) begin
    if (reset) begin
        cancel_next_inst <= 1'b0;
    end
    else if (br_taken && inst_sram_req && inst_sram_addr_ok) begin
        cancel_next_inst <= 1'b1; // inst_sram gets addr_ok when cancel signal comes, cancel the next instruction
    end
    else if (br_taken && !pipe_allowin[0] && !pipe_ready_go[0]) begin
        cancel_next_inst <= 1'b1; // inst_sram is waiting for data_ok when cancel signal comes, cancel the next instruction
    end
    else if (cancel_next_inst && inst_sram_data_ok) begin
        cancel_next_inst <= 1'b0;
    end
end

always @(posedge clk) begin
    if (reset) begin
        br_taken_valid <= 1'b0;
    end
    else if (br_taken) begin
        br_taken_valid <= 1'b1;
    end
    else if (inst_sram_addr_ok) begin
        br_taken_valid <= 1'b0;
    end
end

always @(posedge clk) begin
    if (reset) begin
        br_target_reg <= 32'h0;
    end
    else if (br_taken) begin
        br_target_reg <= br_target;
    end
end

assign seq_pc       = pc + 3'h4;
assign nextpc       = br_taken_valid ? br_target_reg : 
                                                        seq_pc;

assign inst_sram_req    = pipe_allowin[0] && !((ex_WB || has_int_WB) && br_taken);  // instruction memory enable
assign inst_sram_wr     = 1'b0;  // instruction memory write enable
assign inst_sram_wstrb    = 4'b0;  // instruction memory strb
assign inst_sram_size    = 2'b10;  // instruction memory size
assign inst_sram_addr  = nextpc;  // instruction memory address
assign inst_sram_wdata = 32'b0;  // instruction memory write data

assign pipe_ready_go[0] = pipe_valid[0] && (inst_sram_data_ok || inst_IF_reg_valid) && !cancel_next_inst;


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// IF stage to ID stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] inst_ID;
reg  [31:0] pc_ID;
reg         ex_ID;
reg  [ 5:0] csr_ecode_ID; // This signal is used to get the csr_ecode passed to ID stage
wire [ 5:0] csr_ecode_ID_m; // This signal is used to get the csr_ecode in ID stage

always @(posedge clk) begin
    if (pipe_tonext_valid[0]) begin
        inst_ID <= inst;
        pc_ID   <= pc;
        ex_ID   <= ex_IF;
        csr_ecode_ID    <= csr_ecode;
    end
end


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ID stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Pipeline blocking control
// signals for data forwarding
wire ID_stay;
wire ID_stay3;
assign ID_stay   = df_ld_r1_EX   && rf_using1
                || df_ld_r1_MEM  && rf_using1
                || df_ld_r1_WB   && rf_using1
                || df_alu_r1_MEM && rf_using1
                || df_mul_r1_MEM && rf_using1
                || df_csr_r1_MEM && rf_using1
                || df_ld_r2_EX   && rf_using2
                || df_ld_r2_MEM  && rf_using2
                || df_ld_r2_WB   && rf_using2
                || df_alu_r2_MEM && rf_using2
                || df_mul_r2_MEM && rf_using2
                || df_csr_r2_MEM && rf_using2
;
assign ID_stay3  = df_rdcntid_r1_EX && rf_using1
                || df_rdcntid_r1_MEM && rf_using1
                || df_rdcntid_r1_WB && rf_using1
                || df_rdcntid_r2_EX && rf_using2
                || df_rdcntid_r2_MEM && rf_using2
                || df_rdcntid_r2_WB && rf_using2; // Pipeline should stay 3 cycles when rdcntid.w cause the instruction can get the value from the TID csr only in WB stage

reg [1:0] stay_counter; // counter for the number of cycles to stay, used for ID_stay3
always @(posedge clk) begin
    if (reset) begin
        stay_counter <= 2'b00;
    end
    else if (ID_stay3 && stay_counter != 2'b11) begin
        stay_counter <= stay_counter + 1;
    end
    else begin
        stay_counter <= 2'b00;
    end
end

assign pipe_ready_go[1] = pipe_valid[1] && !ID_stay && !ID_stay3 && ~(|stay_counter);

wire rd_eq_r1_EX;
wire rd_eq_r1_MEM;
wire rd_eq_r1_WB;
wire rd_eq_r2_EX;
wire rd_eq_r2_MEM;
wire rd_eq_r2_WB;
wire csreq_EX;
wire csreq_MEM;
wire csreq_WB;

assign rd_eq_r1_EX  = (rf_raddr1 == dest_EX)  && (rf_raddr1 != 5'h0) && pipe_valid[2];
assign rd_eq_r1_MEM = (rf_raddr1 == dest_MEM) && (rf_raddr1 != 5'h0) && pipe_valid[3];
assign rd_eq_r1_WB  = (rf_raddr1 == dest_WB)  && (rf_raddr1 != 5'h0) && pipe_valid[4];
assign rd_eq_r2_EX  = (rf_raddr2 == dest_EX)  && (rf_raddr2 != 5'h0) && pipe_valid[2];
assign rd_eq_r2_MEM = (rf_raddr2 == dest_MEM) && (rf_raddr2 != 5'h0) && pipe_valid[3];
assign rd_eq_r2_WB  = (rf_raddr2 == dest_WB)  && (rf_raddr2 != 5'h0) && pipe_valid[4];
assign csreq_EX     = ((csr_raddr == csr_dest_EX)  || (inst_ertn_EX  && csr_raddr == 14'h0))&& pipe_valid[2];
assign csreq_MEM    = ((csr_raddr == csr_dest_MEM) || (inst_ertn_MEM && csr_raddr == 14'h0))&& pipe_valid[3];
assign csreq_WB     = ((csr_raddr == csr_dest_WB)  || (inst_ertn_WB  && csr_raddr == 14'h0))&& pipe_valid[4];

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
wire df_mul_r1_EX;
wire df_mul_r1_MEM;
wire df_mul_r1_WB;
wire df_mul_r2_EX;
wire df_mul_r2_MEM;
wire df_mul_r2_WB;
wire df_csr_r1_EX;
wire df_csr_r1_MEM;
wire df_csr_r1_WB;
wire df_csr_r2_EX;
wire df_csr_r2_MEM;
wire df_csr_r2_WB;
wire df_csrwr_EX;
wire df_csrwr_MEM;
wire df_csrwr_WB;
wire df_rdcntid_r1_EX;
wire df_rdcntid_r1_MEM;
wire df_rdcntid_r1_WB;
wire df_rdcntid_r2_EX;
wire df_rdcntid_r2_MEM;
wire df_rdcntid_r2_WB;

// exp11: add load/store instructions
assign df_alu_r1_EX  = rd_eq_r1_EX  && gr_we_EX  && !inst_ld_w_EX  && !inst_ld_b_EX  && !inst_ld_h_EX  && !inst_ld_bu_EX  && !inst_ld_hu_EX  && !mul_inst_EX  && !inst_csrrd_EX && !inst_csrwr_EX && !inst_csrxchg_EX;
assign df_alu_r1_MEM = rd_eq_r1_MEM && gr_we_MEM && !inst_ld_w_MEM && !inst_ld_b_MEM && !inst_ld_h_MEM && !inst_ld_bu_MEM && !inst_ld_hu_MEM && !mul_inst_MEM && !inst_csrrd_MEM && !inst_csrwr_MEM && !inst_csrxchg_MEM;
assign df_alu_r1_WB  = rd_eq_r1_WB  && gr_we_WB  && !inst_ld_w_WB  && !inst_ld_b_WB  && !inst_ld_h_WB  && !inst_ld_bu_WB  && !inst_ld_hu_WB  && !mul_inst_WB  && !inst_csrrd_WB && !inst_csrwr_WB && !inst_csrxchg_WB;
assign df_alu_r2_EX  = rd_eq_r2_EX  && gr_we_EX  && !inst_ld_w_EX  && !inst_ld_b_EX  && !inst_ld_h_EX  && !inst_ld_bu_EX  && !inst_ld_hu_EX  && !mul_inst_EX  && !inst_csrrd_EX && !inst_csrwr_EX && !inst_csrxchg_EX;
assign df_alu_r2_MEM = rd_eq_r2_MEM && gr_we_MEM && !inst_ld_w_MEM && !inst_ld_b_MEM && !inst_ld_h_MEM && !inst_ld_bu_MEM && !inst_ld_hu_MEM && !mul_inst_MEM && !inst_csrrd_MEM && !inst_csrwr_MEM && !inst_csrxchg_MEM;
assign df_alu_r2_WB  = rd_eq_r2_WB  && gr_we_WB  && !inst_ld_w_WB  && !inst_ld_b_WB  && !inst_ld_h_WB  && !inst_ld_bu_WB  && !inst_ld_hu_WB  && !mul_inst_WB  && !inst_csrrd_WB && !inst_csrwr_WB && !inst_csrxchg_WB;
assign df_mul_r1_EX  = rd_eq_r1_EX  && gr_we_EX  && !inst_ld_w_EX  && !inst_ld_b_EX  && !inst_ld_h_EX  && !inst_ld_bu_EX  && !inst_ld_hu_EX  &&  mul_inst_EX  ;
assign df_mul_r1_MEM = rd_eq_r1_MEM && gr_we_MEM && !inst_ld_w_MEM && !inst_ld_b_MEM && !inst_ld_h_MEM && !inst_ld_bu_MEM && !inst_ld_hu_MEM &&  mul_inst_MEM ;
assign df_mul_r1_WB  = rd_eq_r1_WB  && gr_we_WB  && !inst_ld_w_WB  && !inst_ld_b_WB  && !inst_ld_h_WB  && !inst_ld_bu_WB  && !inst_ld_hu_WB  &&  mul_inst_WB  ;
assign df_mul_r2_EX  = rd_eq_r2_EX  && gr_we_EX  && !inst_ld_w_EX  && !inst_ld_b_EX  && !inst_ld_h_EX  && !inst_ld_bu_EX  && !inst_ld_hu_EX   &&  mul_inst_EX ;
assign df_mul_r2_MEM = rd_eq_r2_MEM && gr_we_MEM && !inst_ld_w_MEM && !inst_ld_b_MEM && !inst_ld_h_MEM && !inst_ld_bu_MEM && !inst_ld_hu_MEM &&  mul_inst_MEM ;
assign df_mul_r2_WB  = rd_eq_r2_WB  && gr_we_WB  && !inst_ld_w_WB  && !inst_ld_b_WB  && !inst_ld_h_WB  && !inst_ld_bu_WB  && !inst_ld_hu_WB  &&  mul_inst_WB  ;
assign df_ld_r1_EX   = rd_eq_r1_EX  && (inst_ld_w_EX || inst_ld_b_EX || inst_ld_h_EX || inst_ld_bu_EX || inst_ld_hu_EX);
assign df_ld_r1_MEM  = rd_eq_r1_MEM && (inst_ld_w_MEM || inst_ld_b_MEM || inst_ld_h_MEM || inst_ld_bu_MEM || inst_ld_hu_MEM);
assign df_ld_r1_WB   = rd_eq_r1_WB  && (inst_ld_w_WB || inst_ld_b_WB || inst_ld_h_WB || inst_ld_bu_WB || inst_ld_hu_WB);
assign df_ld_r2_EX   = rd_eq_r2_EX  && (inst_ld_w_EX || inst_ld_b_EX || inst_ld_h_EX || inst_ld_bu_EX || inst_ld_hu_EX);
assign df_ld_r2_MEM  = rd_eq_r2_MEM && (inst_ld_w_MEM || inst_ld_b_MEM || inst_ld_h_MEM || inst_ld_bu_MEM || inst_ld_hu_MEM);
assign df_ld_r2_WB   = rd_eq_r2_WB  && (inst_ld_w_WB || inst_ld_b_WB || inst_ld_h_WB || inst_ld_bu_WB || inst_ld_hu_WB);
assign df_csr_r1_EX  = rd_eq_r1_EX  && (inst_csrrd_EX || inst_csrwr_EX || inst_csrxchg_EX);
assign df_csr_r1_MEM = rd_eq_r1_MEM && (inst_csrrd_MEM || inst_csrwr_MEM || inst_csrxchg_MEM);
assign df_csr_r1_WB  = rd_eq_r1_WB  && (inst_csrrd_WB || inst_csrwr_WB || inst_csrxchg_WB);
assign df_csr_r2_EX  = rd_eq_r2_EX  && (inst_csrrd_EX || inst_csrwr_EX || inst_csrxchg_EX);
assign df_csr_r2_MEM = rd_eq_r2_MEM && (inst_csrrd_MEM || inst_csrwr_MEM || inst_csrxchg_MEM);
assign df_csr_r2_WB  = rd_eq_r2_WB  && (inst_csrrd_WB || inst_csrwr_WB || inst_csrxchg_WB);
assign df_csrwr_EX   = csreq_EX  && (inst_csrwr_EX || inst_csrxchg_EX || inst_ertn_EX || inst_syscall_EX);
assign df_csrwr_MEM  = csreq_MEM && (inst_csrwr_MEM || inst_csrxchg_MEM || inst_ertn_MEM || inst_syscall_MEM);
assign df_csrwr_WB   = csreq_WB  && (inst_csrwr_WB || inst_csrxchg_WB || inst_ertn_WB || inst_syscall_WB);
assign df_rdcntid_r1_EX  = rd_eq_r1_EX  && inst_rdcntid_w_EX;
assign df_rdcntid_r1_MEM = rd_eq_r1_MEM && inst_rdcntid_w_MEM;
assign df_rdcntid_r1_WB  = rd_eq_r1_WB  && inst_rdcntid_w_WB;
assign df_rdcntid_r2_EX  = rd_eq_r2_EX  && inst_rdcntid_w_EX;
assign df_rdcntid_r2_MEM = rd_eq_r2_MEM && inst_rdcntid_w_MEM;
assign df_rdcntid_r2_WB  = rd_eq_r2_WB  && inst_rdcntid_w_WB;

assign op_31_26  = inst_ID[31:26];
assign op_25_22  = inst_ID[25:22];
assign op_25_24  = inst_ID[25:24];
assign op_21_20  = inst_ID[21:20];
assign op_19_15  = inst_ID[19:15];
assign op_9_5    = inst_ID[ 9: 5];

assign rd   = inst_ID[ 4: 0];
assign rj   = inst_ID[ 9: 5];
assign rk   = inst_ID[14:10];

assign i12  = inst_ID[21:10];
assign i20  = inst_ID[24: 5];
assign i16  = inst_ID[25:10];
assign i14  = inst_ID[23:10];
assign i26  = {inst_ID[ 9: 0], inst_ID[25:10]};

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));
decoder_2_4  u_dec4(.in(op_25_24 ), .out(op_25_24_d ));
decoder_5_32 u_dec5(.in(op_9_5   ), .out(op_9_5_d   ));

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
assign inst_ld_b      = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
assign inst_ld_h      = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_bu     = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
assign inst_ld_hu     = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_st_w      = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_st_b      = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
assign inst_st_h      = op_31_26_d[6'h0a] & op_25_22_d[4'h5]; // exp11: add load/store instructions
assign inst_jirl      = op_31_26_d[6'h13];
assign inst_b         = op_31_26_d[6'h14];
assign inst_bl        = op_31_26_d[6'h15];
assign inst_beq       = op_31_26_d[6'h16];
assign inst_bne       = op_31_26_d[6'h17];
assign inst_blt       = op_31_26_d[6'h18];
assign inst_bge       = op_31_26_d[6'h19];
assign inst_bltu      = op_31_26_d[6'h1a];
assign inst_bgeu      = op_31_26_d[6'h1b]; // exp11: add branch instructions
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

assign mul_inst = inst_mul_w | inst_mulh_w | inst_mulh_wu;
assign div_inst = inst_div_w | inst_mod_w | inst_div_wu | inst_mod_wu;

// CSR instructions
assign inst_csrrd     = op_31_26_d[6'h01] & op_25_24_d[2'h0] & op_9_5_d[5'h0];
assign inst_csrwr     = op_31_26_d[6'h01] & op_25_24_d[2'h0] & op_9_5_d[5'h1];
assign inst_csrxchg   = op_31_26_d[6'h01] & op_25_24_d[2'h0] & ~op_9_5_d[5'h0] & ~op_9_5_d[5'h1];
assign inst_ertn      = (inst_ID[31:0] == 32'h06483800);
assign inst_syscall   = (inst_ID[31:15] == 17'b1010110);
assign inst_break     = (inst_ID[31:15] == 17'b1010100);
assign inst_rdcntid_w = (inst_ID[31:15] == 17'b0 && inst_ID[14:10] == 5'b11000 && inst_ID[9:5] != 5'b0);
assign inst_rdcntvl_w = (inst_ID[31:15] == 17'b0 && inst_ID[14:10] == 5'b11000 && inst_ID[9:5] == 5'b0);
assign inst_rdcntvh_w = (inst_ID[31:15] == 17'b0 && inst_ID[14:10] == 5'b11001);

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu | inst_st_w | inst_st_b | inst_st_h | inst_jirl | inst_bl | inst_pcaddu12i; // exp11: add branch instructions
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;

// exp10: mul & div
assign mul_op[0]  = inst_mul_w;
assign mul_op[1]  = inst_mulh_w;
assign mul_op[2]  = inst_mulh_wu;

assign div_op[0]  = inst_div_w;
assign div_op[1]  = inst_mod_w;
assign div_op[2]  = inst_div_wu;
assign div_op[3]  = inst_mod_wu;

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu | inst_st_w | inst_st_b | inst_st_h | inst_slti | inst_sltui; // exp11: add load/store instructions
assign need_ui12  =  inst_andi | inst_ori | inst_xori;
assign need_ui14  =  inst_csrrd | inst_csrwr | inst_csrxchg;
assign need_si16  =  inst_jirl | inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu; // exp11: add branch instructions
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_ui12 ? {20'b0, i12[11:0]}         :
             need_ui14 ? {18'b0, i14[13:0]}         :
/*need_ui5 || need_si12*/{{20{i12[11]}}, i12[11:0]} ;

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq |
                       inst_bne | 
                       inst_st_w | 
                       inst_st_b | 
                       inst_st_h | 
                       inst_blt | 
                       inst_bge | 
                       inst_bltu | 
                       inst_bgeu | // exp11: add branch instructions
                       inst_csrrd |
                       inst_csrwr |
                       inst_csrxchg;

assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

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
                       inst_ld_b      |
                       inst_ld_h      |
                       inst_ld_bu     |
                       inst_ld_hu     |
                       inst_st_w      | // store instructions
                       inst_st_b      |
                       inst_st_h      | // exp11: add load/store instructions
                       inst_lu12i_w   | // load upper immediate instructions
                       inst_pcaddu12i |
                       inst_jirl      | // branch instructions
                       inst_bl        ;

assign res_from_mem  = inst_ld_w | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu; // exp11: add load instructions
assign res_from_csr  = inst_csrrd | inst_csrwr | inst_csrxchg;
assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_st_b & ~inst_st_h & ~inst_beq & ~inst_bne & ~inst_b & ~inst_blt & ~inst_bge & ~inst_bltu & ~inst_bgeu & ~inst_ertn & ~ex_ID_m; // debug: gr_we wrong // exp11: add branch instructions & load/store instructions
assign mem_we        = inst_st_w | inst_st_b | inst_st_h; // exp11: add store instructions
assign dest          = dst_is_r1 ? 5'd1 : 
                       inst_rdcntid_w ? rj : // dest of rdcntid.w is rj
                                        rd;
assign csr_dest      = inst_syscall ? 14'h6 : imm[13:0];
assign csr_we    = inst_csrwr || inst_csrxchg;


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

assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd : rk;
assign rf_using1 = inst_beq 
                || inst_bne 
                || inst_blt
                || inst_bge
                || inst_bltu
                || inst_bgeu // exp11: add branch instructions
                || inst_jirl // branch instructions without b/bl
                || inst_ld_w 
                || inst_ld_b
                || inst_ld_h
                || inst_ld_bu
                || inst_ld_hu
                || inst_st_w
                || inst_st_b
                || inst_st_h // load/store instructions // exp11: add load/store instructions
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
                || mul_inst // multiplication instructions
                || div_inst // division instructions
                || inst_slli_w
                || inst_srli_w
                || inst_srai_w
                || inst_slti
                || inst_sltui
                || inst_andi
                || inst_ori
                || inst_xori
                || inst_addi_w // imm instructions
                || inst_csrxchg
;

assign rf_using2 = inst_beq 
                || inst_bne 
                || inst_blt
                || inst_bge
                || inst_bltu
                || inst_bgeu // exp11: add branch instructions
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
                || mul_inst // multiplication instructions
                || div_inst // division instructions
                || inst_sll_w
                || inst_srl_w
                || inst_sra_w // shift instructions without imm instructions
                // || inst_slli_w
                // || inst_srli_w
                // || inst_srai_w
                // || inst_addi_w // imm instructions
                || inst_csrxchg
                || inst_csrrd
                || inst_csrwr
;

assign rj_value  = df_alu_r1_EX  ? alu_result     :
                   df_mul_r1_EX  ? mul_result     :
                   df_csr_r1_EX  ? csr_value_EX   :
                   df_alu_r1_MEM ? alu_result_MEM :
                   df_mul_r1_MEM ? mul_result_MEM :
                   df_ld_r1_MEM  ? mem_result     :
                   df_csr_r1_MEM ? csr_value_MEM  :
                   df_alu_r1_WB  ? alu_result_WB  :
                   df_mul_r1_WB  ? mul_result_WB  :
                   df_ld_r1_WB   ? mem_result_WB  : // exp11 bug is here
                   df_csr_r1_WB  ? csr_value_WB   :
                   rf_rdata1
;

assign rkd_value = df_alu_r2_EX  ? alu_result     :
                   df_mul_r2_EX  ? mul_result     :
                   df_csr_r2_EX  ? csr_value_EX   :
                   df_alu_r2_MEM ? alu_result_MEM :
                   df_mul_r2_MEM ? mul_result_MEM :
                   df_ld_r2_MEM  ? mem_result     :
                   df_csr_r2_MEM ? csr_value_MEM  :
                   df_alu_r2_WB  ? alu_result_WB  :
                   df_mul_r2_WB  ? mul_result_WB  :
                   df_alu_r2_WB  ? alu_result_WB  :
                   df_ld_r2_WB   ? mem_result_WB  : // exp11 bug is here
                   df_csr_r2_WB  ? csr_value_WB   :
                   rf_rdata2
;

assign inst_not_exist = ~inst_add_w & ~inst_sub_w & ~inst_slt & ~inst_sltu 
                      & ~inst_nor & ~inst_and & ~inst_or & ~inst_xor 
                      & ~inst_slli_w & ~inst_srli_w & ~inst_srai_w & ~inst_addi_w 
                      & ~inst_ld_w & ~inst_ld_b & ~inst_ld_h & ~inst_ld_bu & ~inst_ld_hu 
                      & ~inst_st_w & ~inst_st_b & ~inst_st_h 
                      & ~inst_jirl & ~inst_b & ~inst_bl & ~inst_beq & ~inst_bne 
                      & ~inst_blt & ~inst_bge & ~inst_bltu & ~inst_bgeu 
                      & ~inst_lu12i_w & ~inst_slti & ~inst_sltui 
                      & ~inst_andi & ~inst_ori & ~inst_xori 
                      & ~inst_sll_w & ~inst_srl_w & ~inst_sra_w & ~inst_pcaddu12i 
                      & ~inst_mul_w & ~inst_mulh_w & ~inst_mulh_wu 
                      & ~inst_div_w & ~inst_mod_w & ~inst_div_wu & ~inst_mod_wu 
                      & ~inst_csrrd & ~inst_csrwr & ~inst_csrxchg 
                      & ~inst_ertn & ~inst_syscall & ~inst_break
                      & ~inst_rdcntid_w & ~inst_rdcntvl_w & ~inst_rdcntvh_w; // INE exception

assign ex_ID_m = ex_ID | inst_syscall | inst_break | inst_not_exist;


// CSR part
csr_regfile u_csr_regfile(
    .clk    (clk      ),
    .reset    (reset    ),
    .csr_raddr  (csr_raddr),
    .csr_rdata  (csr_rdata),
    .csr_we     (csr_we_real ),
    .csr_waddr  (csr_waddr),
    .csr_wmask  (csr_wmask),
    .csr_wdata  (csr_wdata),
    .wb_pc  (pc_WB    ),
    .wb_vaddr (wb_vaddr),

    .ex_entry (ex_entry),
    .has_int (has_int),
    .hw_int_in (hw_int_in),
    .ipi_int_in (ipi_int_in),
    .coreid_in (coreid_in),
    .ertn_flush   (csr_ertn ),
    .wb_ex  (csr_wbex ),
    .wb_ecode  (csr_ecode_WB),
    .wb_esubcode  (9'h0)
    );
assign csr_we_real = csr_we_WB; // Really need to write CSR
assign csr_raddr = inst_ertn ? 14'b110 :
                   inst_rdcntid_w_WB ? 14'h40 :
                                       imm[13:0];
assign csr_ertn  = inst_ertn;
assign csr_ecode_ID_m = csr_ecode_ID != 6'b0 ? csr_ecode_ID 
                                            : ({6{inst_syscall  }} & 6'hb 
                                            |  {6{inst_break    }} & 6'hc
                                            |  {6{inst_not_exist}} & 6'hd);
// assign csr_wdata = rkd_value_WB;
// assign csr_we    = {32{inst_csrwr_WB}} | {32{inst_csrxchg_WB}} & rj_value_WB;
//csr[waddr] <= ~we & csr[waddr] | we & wdata;
assign csr_value = df_csrwr_EX ? ~({32{inst_csrwr_EX}} | {32{inst_csrxchg_EX}} & rj_value_EX) & csr_rdata |
                                 ({32{inst_csrwr_EX}} | {32{inst_csrxchg_EX}} & rj_value_EX) & rkd_value_EX :
                   df_csrwr_MEM ? ~({32{inst_csrwr_MEM}} | {32{inst_csrxchg_MEM}} & rj_value_MEM) & csr_rdata |
                                  ({32{inst_csrwr_MEM}} | {32{inst_csrxchg_MEM}} & rj_value_MEM) & rkd_value_MEM :
                   df_csrwr_WB  ? ~({32{inst_csrwr_WB }} | {32{inst_csrxchg_WB }} & rj_value_WB ) & csr_rdata |
                                  ({32{inst_csrwr_WB }} | {32{inst_csrxchg_WB }} & rj_value_WB ) & rkd_value_WB  :
                   csr_rdata; 

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
reg  inst_ld_b_EX;
reg  inst_ld_h_EX;
reg  inst_ld_bu_EX;
reg  inst_ld_hu_EX;
reg  inst_st_w_EX;
reg  inst_st_b_EX;
reg  inst_st_h_EX; // exp11: add load/store instructions
reg  inst_jirl_EX;
reg  inst_b_EX;
reg  inst_bl_EX;
reg  inst_beq_EX;
reg  inst_bne_EX;
reg  inst_blt_EX;
reg  inst_bge_EX;
reg  inst_bltu_EX;
reg  inst_bgeu_EX; // exp11: add branch instructions
// reg  inst_lu12i_w_EX;

// exp10: mul & div
reg  mul_inst_EX;
reg  [ 2:0] mul_op_EX;
reg  div_inst_EX;
reg  [ 3:0] div_op_EX;
reg  inst_div_w_EX;
reg  inst_mod_w_EX;
reg  inst_div_wu_EX;
reg  inst_mod_wu_EX;

// exp12: add csr instructions
reg  inst_csrrd_EX;
reg  inst_csrwr_EX;
reg  inst_csrxchg_EX;
reg  inst_ertn_EX;
reg  inst_syscall_EX;
reg  inst_break_EX;
reg  inst_rdcntid_w_EX;
reg  inst_rdcntvl_w_EX;
reg  inst_rdcntvh_w_EX;
reg  ex_EX;
reg  [ 5:0] csr_ecode_EX;
wire ex_EX_m;
wire [ 5:0] csr_ecode_EX_m;

reg  [31:0] imm_EX;
reg  [31:0] br_offs_EX;
reg  [31:0] jirl_offs_EX;
reg         src1_is_pc_EX;
reg         src2_is_imm_EX;

reg         res_from_mem_EX;
reg         res_from_csr_EX;
reg         gr_we_EX;
wire        gr_we_EX_m;
reg         mem_we_EX;
reg  [ 4:0] dest_EX;
reg  [13:0] csr_dest_EX;

reg  [31:0] rj_value_EX;
reg  [31:0] rkd_value_EX;
reg  [31:0] csr_value_EX;
reg         csr_we_EX;

always @(posedge clk) begin
    if(pipe_tonext_valid[1]) begin
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
        inst_ld_b_EX    <= inst_ld_b;
        inst_ld_h_EX    <= inst_ld_h;
        inst_ld_bu_EX   <= inst_ld_bu;
        inst_ld_hu_EX   <= inst_ld_hu;
        inst_st_w_EX    <= inst_st_w;
        inst_st_b_EX    <= inst_st_b;
        inst_st_h_EX    <= inst_st_h; // exp11: add load/store instructions
        inst_jirl_EX    <= inst_jirl;
        inst_b_EX       <= inst_b;
        inst_bl_EX      <= inst_bl;
        inst_beq_EX     <= inst_beq;
        inst_bne_EX     <= inst_bne;
        inst_blt_EX     <= inst_blt;
        inst_bge_EX     <= inst_bge;
        inst_bltu_EX    <= inst_bltu;
        inst_bgeu_EX    <= inst_bgeu; // exp11: add branch instructions
//      inst_lu12i_w_EX <= inst_lu12i_w;
        inst_rdcntid_w_EX <= inst_rdcntid_w;
        inst_rdcntvl_w_EX <= inst_rdcntvl_w;
        inst_rdcntvh_w_EX <= inst_rdcntvh_w;
        ex_EX           <= ex_ID_m;
        imm_EX          <= imm;
        br_offs_EX      <= br_offs;
        jirl_offs_EX    <= jirl_offs;
        src1_is_pc_EX   <= src1_is_pc;
        src2_is_imm_EX  <= src2_is_imm;
        res_from_mem_EX <= res_from_mem;
        res_from_csr_EX <= res_from_csr;
        dest_EX         <= dest;
        csr_dest_EX     <= csr_dest;
        rj_value_EX     <= rj_value;
        rkd_value_EX    <= rkd_value;
        csr_value_EX    <= csr_value;
// exp10: mul & div
        mul_inst_EX     <= mul_inst;
        mul_op_EX       <= mul_op;
        div_op_EX       <= div_op;
        inst_div_w_EX   <= inst_div_w;
        inst_mod_w_EX   <= inst_mod_w;
        inst_div_wu_EX  <= inst_div_wu;
        inst_mod_wu_EX  <= inst_mod_wu;

// exp12: add csr instructions
        inst_csrrd_EX   <= inst_csrrd;
        inst_csrwr_EX   <= inst_csrwr;
        inst_csrxchg_EX <= inst_csrxchg;
        inst_ertn_EX    <= inst_ertn;
        inst_syscall_EX <= inst_syscall;
        inst_break_EX   <= inst_break;
        csr_ecode_EX    <= csr_ecode_ID_m;
    end
end

always @(posedge clk) begin
    if (flush) begin
        gr_we_EX        <= 1'b0;
        mem_we_EX       <= 1'b0;
        csr_we_EX       <= 1'b0;
    end
    else if(pipe_tonext_valid[1]) begin
        gr_we_EX        <= gr_we;
        mem_we_EX       <= mem_we;
        csr_we_EX       <= csr_we;
    end
end

assign gr_we_EX_m = gr_we_EX && ~ex_EX_m;

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
assign rj_l_rd  = ($signed(rj_value_EX) < $signed(rkd_value_EX));
assign rj_lu_rd = (rj_value_EX < rkd_value_EX);
assign rj_geq_rd = !rj_l_rd;
assign rj_gequ_rd = !rj_lu_rd;
assign br_taken = (   inst_beq_EX  &&  rj_eq_rd
                   || inst_bne_EX  && !rj_eq_rd
                   || inst_blt_EX  &&  rj_l_rd
                   || inst_bge_EX  &&  rj_geq_rd
                   || inst_bltu_EX &&  rj_lu_rd
                   || inst_bgeu_EX &&  rj_gequ_rd // exp11: add branch instructions
                   || inst_jirl_EX
                   || inst_bl_EX
                   || inst_b_EX
                   || inst_ertn_EX // exp12: add csr instruction
                  ) && pipe_valid[2] && first_EX
                   || (ex_WB || has_int_WB) && pipe_valid[4];
assign br_target =  (ex_WB || has_int_WB) ? ex_entry : // when there is an exception, jump to the ex_entry
                    (inst_beq_EX || inst_bne_EX || inst_bl_EX || inst_b_EX || inst_blt_EX || inst_bge_EX || inst_bltu_EX || inst_bgeu_EX) ? (pc_EX + br_offs_EX) :
                    (inst_ertn_EX) ? csr_value_EX : 
                    /*inst_jirl*/ (rj_value_EX + jirl_offs_EX); // exp11: add branch instructions
                    
assign alu_src1 = src1_is_pc_EX  ? pc_EX[31:0] : rj_value_EX;
assign alu_src2 = src2_is_imm_EX ? imm_EX : rkd_value_EX;

alu u_alu(
    .alu_op     (alu_op_EX ),
    .alu_src1   (alu_src1  ), // debug:  when instantiating ALU module, alu_src1 & alu_src2 both connect to alu_src2, so connect alu_src1 with alu_src1
    .alu_src2   (alu_src2  ),
    .alu_result (alu_result_original)
    );

// exp10: mul & div
assign mul_src1 = rj_value_EX;
assign mul_src2 = rkd_value_EX;

multiplier u_multiplier(
    .mul_op     (mul_op_EX ),
    .mul_src1   (mul_src1),
    .mul_src2   (mul_src2),
    .mul_result (mul_result)
    );


reg s_div_in_EX; 
always @(posedge clk) begin
    if(reset) begin
        s_div_in_EX <= 1'b0;
    end
    else if((inst_div_w || inst_mod_w) && pipe_tonext_valid[1] && (!br_taken)) begin
        s_div_in_EX <= 1'b1;
    end
    else if(s_div_out_valid) begin
        s_div_in_EX <= 1'b0;
    end
end

reg u_div_in_EX;
always @(posedge clk) begin
    if(reset) begin
        u_div_in_EX <= 1'b0;
    end
    else if((inst_div_wu || inst_mod_wu) && pipe_tonext_valid[1] && (!br_taken)) begin
        u_div_in_EX <= 1'b1;
    end
    else if(u_div_out_valid) begin
        u_div_in_EX <= 1'b0;
    end
end

assign div_src1 = rj_value_EX;
assign div_src2 = rkd_value_EX;

reg div_executing; //??????????????
always @(posedge clk) begin
    if (reset) begin
        div_executing <= 1'b0;
    end
    else if(div_inst_EX && ((s_div_in_EX && s_divisor_ready && s_dividend_ready) || (u_div_in_EX && u_divisor_ready && u_dividend_ready))) begin
        div_executing <= 1'b1;
    end
    else if((s_div_in_EX && s_div_out_valid) || (u_div_in_EX && u_div_out_valid)) begin
        div_executing <= 1'b0;
    end
end

reg div_valid; //?????????????
always @(posedge clk) begin
    if(reset) begin
        div_valid <= 1'b0;
    end
    else if(div_inst_EX && !div_executing) begin
        div_valid <= 1'b1;
    end
    else if(div_inst_EX && ((s_div_in_EX && s_divisor_ready && s_dividend_ready) || (u_div_in_EX && u_divisor_ready && u_dividend_ready))) begin
        div_valid <= 1'b0;
    end
end

assign s_divisor_valid  = div_inst_EX && div_valid;
assign u_divisor_valid  = div_inst_EX && div_valid;
assign s_dividend_valid = div_inst_EX && div_valid;
assign u_dividend_valid = div_inst_EX && div_valid; //?????????????????????????????????????????????????????

// signed division

div_signed u_div_signed(
    .aclk                  (clk             ),
    .s_axis_divisor_tdata  (div_src2        ),
    .s_axis_divisor_tready (s_divisor_ready ),
    .s_axis_divisor_tvalid (s_divisor_valid ),
    .s_axis_dividend_tdata (div_src1        ),
    .s_axis_dividend_tready(s_dividend_ready),
    .s_axis_dividend_tvalid(s_dividend_valid),
    .m_axis_dout_tdata     (s_div_out       ),
    .m_axis_dout_tvalid    (s_div_out_valid )
    );

// unsigned division

div_unsigned u_div_unsigned(
    .aclk                  (clk             ),
    .s_axis_divisor_tdata  (div_src2        ),
    .s_axis_divisor_tready (u_divisor_ready ),
    .s_axis_divisor_tvalid (u_divisor_valid ),
    .s_axis_dividend_tdata (div_src1        ),
    .s_axis_dividend_tready(u_dividend_ready),
    .s_axis_dividend_tvalid(u_dividend_valid),
    .m_axis_dout_tdata     (u_div_out       ),
    .m_axis_dout_tvalid    (u_div_out_valid )
    );

assign div_result = {32{inst_div_w_EX}}  & s_div_out[63:32] | 
                    {32{inst_mod_w_EX}}  & s_div_out[31: 0] |
                    {32{inst_div_wu_EX}} & u_div_out[63:32] | 
                    {32{inst_mod_wu_EX}} & u_div_out[31: 0]
;

assign alu_result = (s_div_in_EX || u_div_in_EX) ? div_result : alu_result_original;

assign cnt_result = inst_rdcntvl_w_EX ? cnt[31:0] : cnt[63:32];

assign addr_unalign = (inst_ld_h_EX || inst_ld_hu_EX || inst_st_h_EX) && alu_result[0] != 1'b0
                   || (inst_ld_w_EX || inst_st_w_EX) && alu_result[1:0] != 2'b0;

assign csr_ecode_EX_m = csr_ecode_EX != 6'b0 ? csr_ecode_EX 
                                            : {6{addr_unalign}} & 6'h9;
assign ex_EX_m = ex_EX | addr_unalign;

reg div_out_valid;

always @(posedge clk) begin
    if(reset) begin
        div_out_valid <= 1'b0;
    end
    else if(div_inst && (!br_taken)) begin
        div_out_valid <= 1'b0;
    end
    else if(s_div_in_EX && s_div_out_valid) begin
        div_out_valid <= 1'b1;
    end
    else if(u_div_in_EX && u_div_out_valid) begin
        div_out_valid <= 1'b1;
    end
    else if((!(s_div_in_EX || u_div_in_EX))) begin
        div_out_valid <= 1'b1;
    end
end

always @(posedge clk) begin
    if(reset) begin
        div_inst_EX     <= 1'b0;
    end
    else if(pipe_tonext_valid[1] && div_inst && (!br_taken)) begin
        div_inst_EX     <= div_inst;
    end
    else if(div_out_valid) begin
        div_inst_EX     <= 1'b0;
    end
end

assign pipe_ready_go[2] = pipe_valid[2] && (div_out_valid || (s_div_in_EX && s_div_out_valid) || (u_div_in_EX && u_div_out_valid) || (!(s_div_in_EX || u_div_in_EX)));

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// EX stage to MEM stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] pc_MEM;
reg  [31:0] alu_result_MEM;
reg  [31:0] cnt_result_MEM;
reg  [31:0] csr_value_MEM;

reg         inst_ld_w_MEM;
reg         inst_ld_b_MEM;
reg         inst_ld_h_MEM;
reg         inst_ld_bu_MEM;
reg         inst_ld_hu_MEM;
reg         inst_st_w_MEM;
reg         inst_st_b_MEM;
reg         inst_st_h_MEM; // exp11: add load/store instructions
reg         inst_rdcntid_w_MEM;
reg         inst_rdcntvl_w_MEM;
reg         inst_rdcntvh_w_MEM;
reg         ex_MEM;

reg  [31:0] rj_value_MEM;
reg  [31:0] rkd_value_MEM;
reg         res_from_mem_MEM;
reg         res_from_csr_MEM; // exp12: add csr instructions
reg         gr_we_MEM;
reg         mem_we_MEM;
reg  [ 4:0] dest_MEM;
reg  [13:0] csr_dest_MEM; 

reg  inst_csrrd_MEM;
reg  inst_csrwr_MEM;
reg  inst_csrxchg_MEM;
reg  inst_ertn_MEM;
reg  inst_syscall_MEM;
reg  [ 5:0] csr_ecode_MEM;
reg  has_int_MEM;
reg  csr_we_MEM;

// exp10: mul
reg  [31:0] mul_result_MEM;
reg         mul_inst_MEM;

always @(posedge clk) begin
    if(pipe_tonext_valid[2]) begin
        pc_MEM           <= pc_EX;
        alu_result_MEM   <= alu_result;
        cnt_result_MEM   <= cnt_result;
        res_from_mem_MEM <= res_from_mem_EX;
        res_from_csr_MEM <= res_from_csr_EX;
        dest_MEM         <= dest_EX;  
        csr_dest_MEM     <= csr_dest_EX;
        inst_ld_w_MEM    <= inst_ld_w_EX;
        inst_ld_b_MEM    <= inst_ld_b_EX;
        inst_ld_h_MEM    <= inst_ld_h_EX;
        inst_ld_bu_MEM   <= inst_ld_bu_EX;
        inst_ld_hu_MEM   <= inst_ld_hu_EX;
        inst_st_w_MEM    <= inst_st_w_EX;
        inst_st_b_MEM    <= inst_st_b_EX;
        inst_st_h_MEM    <= inst_st_h_EX; // exp11: add load/store instructions
        inst_csrrd_MEM   <= inst_csrrd_EX; // exp12: add csr instructions
        inst_csrwr_MEM   <= inst_csrwr_EX; 
        inst_csrxchg_MEM <= inst_csrxchg_EX;
        inst_ertn_MEM    <= inst_ertn_EX;
        inst_syscall_MEM <= inst_syscall_EX;
        inst_rdcntid_w_MEM <= inst_rdcntid_w_EX;
        inst_rdcntvl_w_MEM <= inst_rdcntvl_w_EX;
        inst_rdcntvh_w_MEM <= inst_rdcntvh_w_EX;
        ex_MEM           <= ex_EX_m;
        csr_value_MEM    <= csr_value_EX;
        csr_ecode_MEM    <= csr_ecode_EX_m;
        has_int_MEM      <= has_int;
        rj_value_MEM     <= rj_value_EX;
        rkd_value_MEM    <= rkd_value_EX;
// exp10: mul
        mul_result_MEM   <= mul_result;
        mul_inst_MEM     <= mul_inst_EX;
    end
end

always @(posedge clk) begin
    if (flush) begin
        gr_we_MEM        <= 1'b0;
        csr_we_MEM       <= 1'b0;
        mem_we_MEM       <= 1'b0;
    end
    else if(pipe_tonext_valid[2]) begin
        gr_we_MEM        <= gr_we_EX_m;
        csr_we_MEM       <= csr_we_EX;
        mem_we_MEM       <= mem_we_EX;
    end
end


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MEM stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
reg [3:0] current_state;
reg [3:0] next_state;
localparam NR = 4'b0001, // no request
           WA = 4'b0010, // wait for addr_ok
           WD = 4'b0100, // wait for data_ok
           RD = 4'b1000; // read data from sram

always @(posedge clk) begin
    if(reset || !pipe_valid[3]) begin
        current_state <= NR;
    end
    else begin
        current_state <= next_state;
    end
end

always @(*) begin
    case(current_state)
        NR: 
            if (!data_sram_req) begin
                next_state = NR;
            end
            else if (!data_sram_addr_ok) begin
                next_state = WA;
            end
            else if (data_sram_addr_ok && data_sram_data_ok) begin
                next_state = RD;
            end
            else begin
                next_state = WD;
            end
        WA:
            if (data_sram_addr_ok && !data_sram_data_ok) begin
                next_state = WD;
            end
            else if (data_sram_addr_ok && data_sram_data_ok) begin
                next_state = RD;
            end
            else begin
                next_state = WA;
            end
        WD:
            if (data_sram_data_ok) begin
                next_state = RD;
            end
            else begin
                next_state = WD;
            end
        RD: 
            if (pipe_allowin[4]) begin
                next_state = NR;
            end
            else begin
                next_state = RD;
            end
        default:
            next_state = NR;
    endcase
end



assign data_sram_req   = (inst_ld_w_MEM || inst_ld_b_MEM || inst_ld_h_MEM || inst_ld_bu_MEM 
                       || inst_ld_hu_MEM || inst_st_w_MEM || inst_st_b_MEM || inst_st_h_MEM) 
                       && pipe_valid[3] && ~has_int_MEM && ~ex_MEM && ~has_int_WB && ~ex_WB
                       && (current_state == NR || current_state == WA);
assign data_sram_addr  = alu_result_MEM;
assign data_sram_wstrb    = (inst_st_b_MEM ? (4'h1 << data_sram_addr[1:0]) :
                          inst_st_h_MEM ? (4'h3 << data_sram_addr[1:0]) :
                                          4'hf) 
                        & {4{mem_we_MEM && pipe_valid[3] && ~has_int_MEM && ~ex_MEM && ~has_int_WB && ~ex_WB}}; // If has exception in the pipeline, do not write to data_sram
assign data_sram_wr = |data_sram_wstrb;
assign data_sram_size = inst_ld_h_MEM || inst_ld_hu_MEM || inst_st_h_MEM ? 2'b01 :
                        inst_ld_b_MEM || inst_ld_bu_MEM || inst_st_b_MEM ? 2'b00 :
                        2'b10;
assign data_sram_wdata = inst_st_b_MEM ? {4{rkd_value_MEM[ 7:0]}} :
                         inst_st_h_MEM ? {2{rkd_value_MEM[15:0]}} :
                                         rkd_value_MEM;

assign mem_result   = inst_ld_b_MEM ? data_sram_addr[1:0] == 2'h0 ? {{24{data_sram_rdata[ 7]}}, data_sram_rdata[ 7: 0]} :
                                      data_sram_addr[1:0] == 2'h1 ? {{24{data_sram_rdata[15]}}, data_sram_rdata[15: 8]} :
                                      data_sram_addr[1:0] == 2'h2 ? {{24{data_sram_rdata[23]}}, data_sram_rdata[23:16]} :
                                                                        {{24{data_sram_rdata[31]}}, data_sram_rdata[31:24]} :
                      inst_ld_h_MEM ? data_sram_addr[1:0] == 2'h0 ? {{16{data_sram_rdata[15]}}, data_sram_rdata[15: 0]} :
                                                                        {{16{data_sram_rdata[31]}}, data_sram_rdata[31:16]} :
                      inst_ld_bu_MEM ? data_sram_addr[1:0] == 2'h0 ? {{24'b0, data_sram_rdata[ 7: 0]}} :
                                       data_sram_addr[1:0] == 2'h1 ? {{24'b0, data_sram_rdata[15: 8]}} :
                                       data_sram_addr[1:0] == 2'h2 ? {{24'b0, data_sram_rdata[23:16]}} :
                                                                         {{24'b0, data_sram_rdata[31:24]}} :
                      inst_ld_hu_MEM ? data_sram_addr[1:0] == 2'h0 ? {{16'b0, data_sram_rdata[15: 0]}} :
                                                                         {{16'b0, data_sram_rdata[31:16]}} :
                                                                          data_sram_rdata;

assign pipe_ready_go[3] = pipe_valid[3] && (current_state == RD || current_state == NR && !data_sram_req);


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MEM stage to WB stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


reg  [31:0] pc_WB;
reg  [31:0] alu_result_WB;
reg  [31:0] mem_result_WB;
reg  [31:0] cnt_result_WB;
reg  [31:0] csr_value_WB;

reg         inst_ld_w_WB;
reg         inst_ld_b_WB;
reg         inst_ld_h_WB;
reg         inst_ld_bu_WB;
reg         inst_ld_hu_WB;
reg         inst_st_w_WB;
reg         inst_st_b_WB;
reg         inst_st_h_WB; // exp11: add load/store instructions
reg         inst_csrrd_WB; // exp12: add csr instructions
reg         inst_csrwr_WB;
reg         inst_csrxchg_WB;
reg         inst_ertn_WB;
reg         inst_syscall_WB;
reg         inst_rdcntid_w_WB;
reg         inst_rdcntvl_w_WB;
reg         inst_rdcntvh_w_WB;
reg         ex_WB;

reg  [ 5:0] csr_ecode_WB;
reg  has_int_WB;
reg  [31:0] rj_value_WB;
reg  [31:0] rkd_value_WB;
reg         res_from_mem_WB;
reg         res_from_csr_WB; // exp12: add csr instructions
reg         gr_we_WB;
reg  [ 4:0] dest_WB;
reg  [13:0] csr_dest_WB;
reg         csr_we_WB;

// exp10: mul
reg  [31:0] mul_result_WB;
reg         mul_inst_WB;

wire [31:0] wb_vaddr;

always @(posedge clk) begin
    if (reset) begin
        pc_WB           <= 32'h0;
//        alu_result_WB   <= 32'h0;
//        mem_result_WB   <= 32'h0;
        res_from_mem_WB <= 1'b0;
        res_from_csr_WB <= 1'b0;
        dest_WB         <= 5'h0;
        csr_dest_WB     <= 14'h0;
    end
    else if(pipe_tonext_valid[3]) begin
        pc_WB           <= pc_MEM;
        alu_result_WB   <= alu_result_MEM;
        mem_result_WB   <= mem_result;
        cnt_result_WB   <= cnt_result_MEM;
        res_from_mem_WB <= res_from_mem_MEM;
        res_from_csr_WB <= res_from_csr_MEM;
        dest_WB         <= dest_MEM;
        csr_dest_WB     <= csr_dest_MEM;
        inst_ld_w_WB    <= inst_ld_w_MEM;
        inst_ld_b_WB    <= inst_ld_b_MEM;
        inst_ld_h_WB    <= inst_ld_h_MEM;
        inst_ld_bu_WB   <= inst_ld_bu_MEM;
        inst_ld_hu_WB   <= inst_ld_hu_MEM;
        inst_st_w_WB    <= inst_st_w_MEM;
        inst_st_b_WB    <= inst_st_b_MEM;
        inst_st_h_WB    <= inst_st_h_MEM; // exp11: add load/store instructions
        inst_csrrd_WB   <= inst_csrrd_MEM; // exp12: add csr instructions
        inst_csrwr_WB   <= inst_csrwr_MEM; 
        inst_csrxchg_WB <= inst_csrxchg_MEM;
        inst_ertn_WB    <= inst_ertn_MEM;
        inst_syscall_WB <= inst_syscall_MEM;
        inst_rdcntid_w_WB <= inst_rdcntid_w_MEM;
        inst_rdcntvl_w_WB <= inst_rdcntvl_w_MEM;
        inst_rdcntvh_w_WB <= inst_rdcntvh_w_MEM;
        csr_value_WB    <= csr_value_MEM;
        csr_ecode_WB    <= csr_ecode_MEM;
        ex_WB           <= ex_MEM;
        has_int_WB      <= has_int_MEM;
        rj_value_WB     <= rj_value_MEM;
        rkd_value_WB    <= rkd_value_MEM;
// exp10: mul
        mul_result_WB   <= mul_result_MEM;
        mul_inst_WB     <= mul_inst_MEM;
    end
end

always @(posedge clk) begin
    if (reset) begin
        gr_we_WB        <= 1'b0;
    end
    else if (flush) begin
        gr_we_WB        <= 1'b0;
        csr_we_WB          <= 1'b0;
    end
    else if(pipe_tonext_valid[3]) begin
        gr_we_WB        <= gr_we_MEM;
        csr_we_WB       <= csr_we_MEM;
    end
end

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// WB stage
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// exp10: mul
assign final_result = res_from_mem_WB ? mem_result_WB :
                      mul_inst_WB     ? mul_result_WB :
                      res_from_csr_WB ? csr_value_WB :
                      inst_rdcntid_w_WB ? csr_value :
                      inst_rdcntvh_w_WB || inst_rdcntvl_w_WB ? cnt_result_WB :
                      alu_result_WB;

assign rf_we    = gr_we_WB && pipe_valid[4];
assign rf_waddr = dest_WB;
assign rf_wdata = final_result;

assign pipe_ready_go[4] = pipe_valid[4];

assign csr_waddr = csr_dest_WB;
assign csr_wmask = {32{inst_csrwr_WB}} | {32{inst_csrxchg_WB}} & rj_value_WB;
assign csr_wdata = rkd_value_WB;
assign csr_wbex  = (ex_WB || has_int_WB) && csr_wbex_rst;

// Reset the csr_wbex signal to ensure csr_wbex only lasts for one cycle
always @(posedge clk) begin
    if (reset) begin
        csr_wbex_rst <= 1'b1;
    end
    else if (ex_WB || has_int_WB) begin
        csr_wbex_rst <= 1'b0;
    end
    else if (~ex_WB && ~has_int_WB) begin
        csr_wbex_rst <= 1'b1;
    end
end

assign wb_vaddr = alu_result_WB;

// debug info generate
assign debug_wb_pc       = pc_WB;
assign debug_wb_rf_we    = {4{rf_we}};  // debug: it should be debug_wb_rf_we
assign debug_wb_rf_wnum  = dest_WB;
assign debug_wb_rf_wdata = final_result;

endmodule


// exp10 bug: 
// 1. something wrong with the logic of pipeline: missing some conditions of signal transmission between EX and MEM stage
// 2. something wrong with the instantiation of divider module
// 3. something wrong with the handshake signal logic of divider

// exp11 bug:
// 1. a datapath bug: when the instruction conflicts with a load instruction in WB stage, the register value should be mem_result_WB


// exp13 bug:
// 1. signal last time bug: Maybe you can be confused by the signal csr_wbex_rst/flush_rst, these signals are used to reset the signal csr_wbex/flush. In simulation, the signal csr_wbex/flush signals can last for more than one cycle, then cause some CSR/registers to be written more than once. Therefore, we cannot get the correct result. So we need to ensure that the signal csr_wbex/flush only last for one cycle to avoid this problem.
// 2. datapath bug: Since instruction rdcntid.w get its value in the WB stage, if the instruction conflicts with other instructions in the pipeline, disastrous consequences will occur. Therefore, we need to block the pipeline when the instruction rdcntid.w is conflict with other instructions for three cycles to ensure that the instructions after rdcntid.w can get the correct value from the register file.