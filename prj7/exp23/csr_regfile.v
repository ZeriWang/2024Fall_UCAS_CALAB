module csr_regfile(
    input        clk,
    input        reset,

    // instruction interface
    input  [13:0] csr_raddr,
    output [31:0] csr_rdata,
    input         csr_we,
    input  [13:0] csr_waddr,
    input  [31:0] csr_wmask,
    input  [31:0] csr_wdata,
    input  [31:0] wb_pc,
    input  [31:0] wb_vaddr,

    // hardware interface
    output [31:0] ex_entry,
    output        has_int,
    input         ertn_flush,
    input  [ 7:0] hw_int_in,
    input         ipi_int_in,
    input  [31:0] coreid_in,
    input         wb_ex,
    input  [ 5:0] wb_ecode,
    input  [ 8:0] wb_esubcode,
    input         ex_from_IF,
    input         tlb_ex_WB,
    output [ 5:0] estat_ecode,
    output [ 9:0] asid_asid,
    output [18:0] tlbehi_vppn,
    output [ 3:0] tlbidx_index,
    output [ 5:0] tlbidx_ps,
    output        tlbidx_ne,
    output [31:0] tlbelo0_rdata,
    output [31:0] tlbelo1_rdata,
    input         asid_asid_we,
    input         tlbidx_index_we,
    input         tlbidx_ps_we,
    input         tlbidx_ne_we,
    input         tlbe_we,
    input  [ 9:0] asid_asid_wdata,
    input  [ 3:0] tlbidx_index_wdata,
    input  [ 5:0] tlbidx_ps_wdata,
    input         tlbidx_ne_wdata,
    input  [18:0] tlbehi_vppn_wdata,
    input  [31:0] tlbelo0_wdata,
    input  [31:0] tlbelo1_wdata,
    output [31:0] tlbr_entry,
    output        crmd_pg,
    output [ 1:0] crmd_plv,
    output [ 1:0] crmd_datm,
    output [31:0] dmw0, dmw1
);

`define CSR_CRMD 14'h0
`define CSR_PRMD 14'h1
`define CSR_ECFG 14'h4
`define CSR_ESTAT 14'h5
`define CSR_ERA 14'h6
`define CSR_BADV 14'h7
`define CSR_EENTRY 14'hc
`define CSR_SAVE0 14'h30
`define CSR_SAVE1 14'h31
`define CSR_SAVE2 14'h32
`define CSR_SAVE3 14'h33
`define CSR_TID 14'h40
`define CSR_TCFG 14'h41
`define CSR_TVAL 14'h42
`define CSR_TICLR 14'h44
`define CSR_DMW0 14'h180
`define CSR_DMW1 14'h181
`define CSR_ASID 14'h18
`define CSR_TLBIDX 14'h10
`define CSR_TLBEHI 14'h11
`define CSR_TLBELO0 14'h12
`define CSR_TLBELO1 14'h13
`define CSR_TLBRENTRY 14'h88

`define CSR_CRMD_PLV 1:0
`define CSR_CRMD_IE 2
`define CSR_CRMD_DA 3
`define CSR_CRMD_PG 4
`define CSR_CRMD_DATF 6:5
`define CSR_CRMD_DATM 8:7
`define CSR_PRMD_PPLV 1:0
`define CSR_PRMD_PIE 2
`define CSR_ECFG_LIE 12:0
`define CSR_ESTAT_IS10 1:0
`define CSR_TICLR_CLR 0
`define CSR_ERA_PC 31:0
`define CSR_EENTRY_VA 31:6
`define CSR_SAVE_DATA 31:0
`define CSR_TID_TID 31:0
`define CSR_TCFG_EN 0
`define CSR_TCFG_PERIOD 1
`define CSR_TCFG_INITV 31:2
`define CSR_TLBIDX_INDEX 3:0
`define CSR_TLBIDX_PS 29:24
`define CSR_TLBIDX_NE 31
`define CSR_TLBEHI_VPPN 31:13
`define CSR_TLBELO0_V 0
`define CSR_TLBELO0_D 1
`define CSR_TLBELO0_PLV 3:2
`define CSR_TLBELO0_MAT 5:4
`define CSR_TLBELO0_G 6
`define CSR_TLBELO0_PPN 27:8
`define CSR_TLBELO1_V 0
`define CSR_TLBELO1_D 1
`define CSR_TLBELO1_PLV 3:2
`define CSR_TLBELO1_MAT 5:4
`define CSR_TLBELO1_G 6
`define CSR_TLBELO1_PPN 27:8
`define CSR_ASID_ASID 9:0
`define CSR_ASID_ASIDBITS 23:16
`define CSR_TLBRENTRY_PA 31:6

`define CSR_DMW_VSEG 31:29
`define CSR_DMW_PSEG 27:25
`define CSR_DMW_MAT 5:4
`define CSR_DMW_PLV3 3
`define CSR_DMW_PLV0 0

`define ECODE_ADE 6'h8
`define ECODE_ALE 6'h9
`define ECODE_TLBR 6'h3f
`define ECODE_PIL 6'h01
`define ECODE_PIS 6'h02
`define ECODE_PIF 6'h03
`define ECODE_PME 6'h04
`define ECODE_PPI 6'h07
`define ESUBCODE_ADEF 9'h0
// wire [31:0] crmd;
// wire [31:0] prmd;
// wire [31:0] estat;
// wire [31:0] era;
// wire [31:0] eentry;
// wire [31:0] save0;
// wire [31:0] save1;
// wire [31:0] save2;
// wire [31:0] save3;
reg [1:0] csr_crmd_plv;
reg       csr_crmd_ie;
reg       csr_crmd_da;
reg       csr_crmd_pg;
reg [1:0] csr_crmd_datf;
reg [1:0] csr_crmd_datm;

reg [1:0] csr_prmd_pplv;
reg       csr_prmd_pie;

reg [12:0] csr_ecfg_lie;

reg [12:0] csr_estat_is;
reg [ 5:0] csr_estat_ecode;
reg [ 8:0] csr_estat_esubcode;

reg [31:0] csr_era_pc;

wire       wb_ex_addr_err;
reg [31:0] csr_badv_vaddr;

reg [25:0] csr_eentry_va;

reg [31:0] csr_save0_data;
reg [31:0] csr_save1_data;
reg [31:0] csr_save2_data;
reg [31:0] csr_save3_data;

reg [31:0] csr_tid_tid;

reg         csr_tcfg_en;
reg         csr_tcfg_periodic;
reg  [29:0] csr_tcfg_initval;
wire [31:0] tcfg_next_value;
wire [31:0] csr_tval;
reg  [31:0] timer_cnt;

wire        csr_ticlr_clr;

reg [ 3:0] csr_tlbidx_index;
reg [ 5:0] csr_tlbidx_ps;
reg        csr_tlbidx_ne;

reg [18:0] csr_tlbehi_vppn;

reg        csr_tlbelo0_v;
reg        csr_tlbelo0_d;
reg [ 1:0] csr_tlbelo0_plv;
reg [ 1:0] csr_tlbelo0_mat;
reg        csr_tlbelo0_g;
reg [19:0] csr_tlbelo0_ppn;

reg        csr_tlbelo1_v;
reg        csr_tlbelo1_d;
reg [ 1:0] csr_tlbelo1_plv;
reg [ 1:0] csr_tlbelo1_mat;
reg        csr_tlbelo1_g;
reg [19:0] csr_tlbelo1_ppn;
reg [25:0] csr_tlbrentry_pa;

reg  [ 9:0] csr_asid_asid;
wire [ 7:0] csr_asid_asidbits;

reg        csr_dmw0_plv0;
reg        csr_dmw0_plv3;
reg [ 1:0] csr_dmw0_mat;
reg [ 2:0] csr_dmw0_pseg;
reg [ 2:0] csr_dmw0_vseg;

reg        csr_dmw1_plv0;
reg        csr_dmw1_plv3;
reg [ 1:0] csr_dmw1_mat;
reg [ 2:0] csr_dmw1_pseg;
reg [ 2:0] csr_dmw1_vseg;

// CRMD
always @(posedge clk) begin
    if (reset)
        csr_crmd_plv <= 2'b0;
    else if (wb_ex)
        csr_crmd_plv <= 2'b0;
    else if (ertn_flush)
        csr_crmd_plv <= csr_prmd_pplv;
    else if (csr_we && csr_waddr==`CSR_CRMD)
        csr_crmd_plv <= csr_wmask[`CSR_CRMD_PLV] & csr_wdata[`CSR_CRMD_PLV] 
                     | ~csr_wmask[`CSR_CRMD_PLV] & csr_crmd_plv;
end

always @(posedge clk) begin
    if (reset) begin
        csr_crmd_ie <= 1'b0;
        csr_crmd_da <= 1'b1;
        csr_crmd_pg <= 1'b0;
        csr_crmd_datf <= 2'b0;
        csr_crmd_datm <= 2'b0;
    end
    else if (wb_ex) begin
        csr_crmd_ie <= 1'b0;
        if (wb_ecode == 6'h3f) begin
			csr_crmd_da <= 1'd1;
			csr_crmd_pg <= 1'd0;
		end
    end
    else if (ertn_flush) begin
        csr_crmd_ie <= csr_prmd_pie;
        if (csr_estat_ecode == 6'h3f) begin
			csr_crmd_da <= 1'd0;
			csr_crmd_pg <= 1'd1;
		end
    end
    else if (csr_we && csr_waddr==`CSR_CRMD) begin
        csr_crmd_ie <= csr_wmask[`CSR_CRMD_IE] & csr_wdata[`CSR_CRMD_IE]
                    | ~csr_wmask[`CSR_CRMD_IE] & csr_crmd_ie;
        csr_crmd_da <= csr_wmask[`CSR_CRMD_DA] & csr_wdata[`CSR_CRMD_DA]
                    | ~csr_wmask[`CSR_CRMD_DA] & csr_crmd_da;
        csr_crmd_pg <= csr_wmask[`CSR_CRMD_PG] & csr_wdata[`CSR_CRMD_PG]
                    | ~csr_wmask[`CSR_CRMD_PG] & csr_crmd_pg;
        csr_crmd_datf <= csr_wmask[`CSR_CRMD_DATF] & csr_wdata[`CSR_CRMD_DATF]
                    | ~csr_wmask[`CSR_CRMD_DATF] & csr_crmd_datf;
        csr_crmd_datm <= csr_wmask[`CSR_CRMD_DATM] & csr_wdata[`CSR_CRMD_DATM]
                    | ~csr_wmask[`CSR_CRMD_DATM] & csr_crmd_datm;
    end
end

// PRMD
always @(posedge clk) begin
    if (wb_ex) begin
        csr_prmd_pplv <= csr_crmd_plv;
        csr_prmd_pie <= csr_crmd_ie;
    end
    else if (csr_we && csr_waddr==`CSR_PRMD) begin
        csr_prmd_pplv <= csr_wmask[`CSR_PRMD_PPLV] & csr_wdata[`CSR_PRMD_PPLV]
                      | ~csr_wmask[`CSR_PRMD_PPLV] & csr_prmd_pplv;
        csr_prmd_pie <= csr_wmask[`CSR_PRMD_PIE] & csr_wdata[`CSR_PRMD_PIE]
                     | ~csr_wmask[`CSR_PRMD_PIE] & csr_prmd_pie;
    end
end

// ECFG
always @(posedge clk) begin
    if (reset)
        csr_ecfg_lie <= 13'b0;
    else if (csr_we && csr_waddr==`CSR_ECFG)
        csr_ecfg_lie <= csr_wmask[`CSR_ECFG_LIE] & 13'h1bff & csr_wdata[`CSR_ECFG_LIE]
                     | ~csr_wmask[`CSR_ECFG_LIE] & 13'h1bff & csr_ecfg_lie;
end

// ESTAT
always @(posedge clk) begin
    if (reset) begin
        csr_estat_is[1:0] <= 2'b0;
        csr_estat_is[11]  <= 1'b0;
    end
    else if (csr_we && csr_waddr==`CSR_ESTAT)
        csr_estat_is[1:0] <= csr_wmask[`CSR_ESTAT_IS10] & csr_wdata[`CSR_ESTAT_IS10]
                          | ~csr_wmask[`CSR_ESTAT_IS10] & csr_estat_is[1:0];

    csr_estat_is[9:2] <= hw_int_in[7:0];
    csr_estat_is[10] <= 1'b0;

    if (timer_cnt[31:0]==32'b0)
        csr_estat_is[11] <= 1'b1;
    else if (csr_we && csr_waddr==`CSR_TICLR && csr_wmask[`CSR_TICLR_CLR]
    && csr_wdata[`CSR_TICLR_CLR])
        csr_estat_is[11] <= 1'b0;

    csr_estat_is[12] <= ipi_int_in;
end

always @(posedge clk) begin
    if (wb_ex) begin
        csr_estat_ecode <= wb_ecode;
        csr_estat_esubcode <= wb_esubcode;
    end
end

// ERA
always @(posedge clk) begin
    if (wb_ex)
        csr_era_pc <= wb_pc;
    else if (csr_we && csr_waddr==`CSR_ERA)
        csr_era_pc <= csr_wmask[`CSR_ERA_PC] & csr_wdata[`CSR_ERA_PC]
                   | ~csr_wmask[`CSR_ERA_PC] & csr_era_pc;
end

// BADV
assign wb_ex_addr_err = wb_ecode==`ECODE_ADE || wb_ecode==`ECODE_ALE || tlb_ex_WB;
always @(posedge clk) begin
    if (wb_ex && wb_ex_addr_err)
        csr_badv_vaddr <= (wb_ecode==`ECODE_ADE &&
                           wb_esubcode==`ESUBCODE_ADEF || ex_from_IF) ? wb_pc : wb_vaddr;
end

// EENTRY
always @(posedge clk) begin
    if (csr_we && csr_waddr==`CSR_EENTRY)
        csr_eentry_va <= csr_wmask[`CSR_EENTRY_VA] & csr_wdata[`CSR_EENTRY_VA]
                      | ~csr_wmask[`CSR_EENTRY_VA] & csr_eentry_va;
end

// TLBRENTRY
always @(posedge clk) begin
    if (csr_we && csr_waddr==`CSR_TLBRENTRY)
        csr_tlbrentry_pa <= csr_wmask[`CSR_TLBRENTRY_PA] & csr_wdata[`CSR_TLBRENTRY_PA]
                         | ~csr_wmask[`CSR_TLBRENTRY_PA] & csr_tlbrentry_pa;
end

// SAVE0~3
always @(posedge clk) begin
    if (csr_we && csr_waddr==`CSR_SAVE0)
        csr_save0_data <= csr_wmask[`CSR_SAVE_DATA] & csr_wdata[`CSR_SAVE_DATA]
                       | ~csr_wmask[`CSR_SAVE_DATA] & csr_save0_data;
    if (csr_we && csr_waddr==`CSR_SAVE1)
        csr_save1_data <= csr_wmask[`CSR_SAVE_DATA] & csr_wdata[`CSR_SAVE_DATA]
                       | ~csr_wmask[`CSR_SAVE_DATA] & csr_save1_data;
    if (csr_we && csr_waddr==`CSR_SAVE2)
        csr_save2_data <= csr_wmask[`CSR_SAVE_DATA] & csr_wdata[`CSR_SAVE_DATA]
                       | ~csr_wmask[`CSR_SAVE_DATA] & csr_save2_data;
    if (csr_we && csr_waddr==`CSR_SAVE3)
        csr_save3_data <= csr_wmask[`CSR_SAVE_DATA] & csr_wdata[`CSR_SAVE_DATA]
                       | ~csr_wmask[`CSR_SAVE_DATA] & csr_save3_data;
end

// TID
always @(posedge clk) begin
    if (reset)
        csr_tid_tid <= coreid_in;
    else if (csr_we && csr_waddr==`CSR_TID)
        csr_tid_tid <= csr_wmask[`CSR_TID_TID] & csr_wdata[`CSR_TID_TID]
                    | ~csr_wmask[`CSR_TID_TID] & csr_tid_tid;
end

// TCFG & TVAL
always @(posedge clk) begin
    if (reset)
        csr_tcfg_en <= 1'b0;
    else if (csr_we && csr_waddr==`CSR_TCFG)
        csr_tcfg_en <= csr_wmask[`CSR_TCFG_EN] & csr_wdata[`CSR_TCFG_EN]
                    | ~csr_wmask[`CSR_TCFG_EN] & csr_tcfg_en;
    if (csr_we && csr_waddr==`CSR_TCFG) begin
        csr_tcfg_periodic <= csr_wmask[`CSR_TCFG_PERIOD] & csr_wdata[`CSR_TCFG_PERIOD]
                          | ~csr_wmask[`CSR_TCFG_PERIOD] & csr_tcfg_periodic;
        csr_tcfg_initval <= csr_wmask[`CSR_TCFG_INITV] & csr_wdata[`CSR_TCFG_INITV]
                         | ~csr_wmask[`CSR_TCFG_INITV] & csr_tcfg_initval;
    end
end

assign tcfg_next_value = csr_wmask[31:0] & csr_wdata[31:0]
                      | ~csr_wmask[31:0] & {csr_tcfg_initval,
csr_tcfg_periodic, csr_tcfg_en};
always @(posedge clk) begin
    if (reset)
        timer_cnt <= 32'hffffffff;
    else if (csr_we && csr_waddr==`CSR_TCFG && tcfg_next_value[`CSR_TCFG_EN])
        timer_cnt <= {tcfg_next_value[`CSR_TCFG_INITV], 2'b0};
    else if (csr_tcfg_en && timer_cnt!=32'hffffffff) begin
        if (timer_cnt[31:0]==32'b0 && csr_tcfg_periodic)
            timer_cnt <= {csr_tcfg_initval, 2'b0};
        else
            timer_cnt <= timer_cnt - 1'b1;
    end
end
assign csr_tval = timer_cnt[31:0];

// TICLR
assign csr_ticlr_clr = 1'b0;

// TLBIDX
always @(posedge clk) begin
    if (tlbidx_index_we)
        csr_tlbidx_index <= tlbidx_index_wdata;
    else if (csr_we && csr_waddr==`CSR_TLBIDX)
        csr_tlbidx_index <= csr_wmask[`CSR_TLBIDX_INDEX] & csr_wdata[`CSR_TLBIDX_INDEX]
                         | ~csr_wmask[`CSR_TLBIDX_INDEX] & csr_tlbidx_index;

    if (tlbidx_ps_we)
        csr_tlbidx_ps <= tlbidx_ps_wdata;
    else if (csr_we && csr_waddr==`CSR_TLBIDX)
        csr_tlbidx_ps <= csr_wmask[`CSR_TLBIDX_PS] & csr_wdata[`CSR_TLBIDX_PS]
                      | ~csr_wmask[`CSR_TLBIDX_PS] & csr_tlbidx_ps;

    if (tlbidx_ne_we)
        csr_tlbidx_ne <= tlbidx_ne_wdata;
    else if (csr_we && csr_waddr==`CSR_TLBIDX)
        csr_tlbidx_ne <= csr_wmask[`CSR_TLBIDX_NE] & csr_wdata[`CSR_TLBIDX_NE]
                      | ~csr_wmask[`CSR_TLBIDX_NE] & csr_tlbidx_ne;
end

// ASID
always @(posedge clk) begin
    if (asid_asid_we)
        csr_asid_asid <= asid_asid_wdata;
    else if (csr_we && csr_waddr==`CSR_ASID)
        csr_asid_asid <= csr_wmask[`CSR_ASID_ASID] & csr_wdata[`CSR_ASID_ASID]
                      | ~csr_wmask[`CSR_ASID_ASID] & csr_asid_asid;
end
assign csr_asid_asidbits = 8'd10;

// TLBEHI
always @(posedge clk) begin
    if (tlbe_we || tlb_ex_WB)
        csr_tlbehi_vppn <= tlbehi_vppn_wdata;
    else if (csr_we && csr_waddr==`CSR_TLBEHI)
        csr_tlbehi_vppn <= csr_wmask[`CSR_TLBEHI_VPPN] & csr_wdata[`CSR_TLBEHI_VPPN]
                        | ~csr_wmask[`CSR_TLBEHI_VPPN] & csr_tlbehi_vppn;
end

// TLBELO0
always @(posedge clk) begin
    if (tlbe_we) begin
        csr_tlbelo0_v <= tlbelo0_wdata[`CSR_TLBELO0_V];
        csr_tlbelo0_d <= tlbelo0_wdata[`CSR_TLBELO0_D];
        csr_tlbelo0_plv <= tlbelo0_wdata[`CSR_TLBELO0_PLV];
        csr_tlbelo0_mat <= tlbelo0_wdata[`CSR_TLBELO0_MAT];
        csr_tlbelo0_g <= tlbelo0_wdata[`CSR_TLBELO0_G];
        csr_tlbelo0_ppn <= tlbelo0_wdata[`CSR_TLBELO0_PPN];
    end
    else if (csr_we && csr_waddr==`CSR_TLBELO0) begin
        csr_tlbelo0_v <= csr_wmask[`CSR_TLBELO0_V] & csr_wdata[`CSR_TLBELO0_V]
                      | ~csr_wmask[`CSR_TLBELO0_V] & csr_tlbelo0_v;
        csr_tlbelo0_d <= csr_wmask[`CSR_TLBELO0_D] & csr_wdata[`CSR_TLBELO0_D]
                      | ~csr_wmask[`CSR_TLBELO0_D] & csr_tlbelo0_d;
        csr_tlbelo0_plv <= csr_wmask[`CSR_TLBELO0_PLV] & csr_wdata[`CSR_TLBELO0_PLV]
                        | ~csr_wmask[`CSR_TLBELO0_PLV] & csr_tlbelo0_plv;
        csr_tlbelo0_mat <= csr_wmask[`CSR_TLBELO0_MAT] & csr_wdata[`CSR_TLBELO0_MAT]
                         | ~csr_wmask[`CSR_TLBELO0_MAT] & csr_tlbelo0_mat;
        csr_tlbelo0_g <= csr_wmask[`CSR_TLBELO0_G] & csr_wdata[`CSR_TLBELO0_G]
                         | ~csr_wmask[`CSR_TLBELO0_G] & csr_tlbelo0_g;
        csr_tlbelo0_ppn <= csr_wmask[`CSR_TLBELO0_PPN] & csr_wdata[`CSR_TLBELO0_PPN]
                         | ~csr_wmask[`CSR_TLBELO0_PPN] & csr_tlbelo0_ppn;
    end
end

// TLBELO1
always @(posedge clk) begin
    if (tlbe_we) begin
        csr_tlbelo1_v <= tlbelo1_wdata[`CSR_TLBELO1_V];
        csr_tlbelo1_d <= tlbelo1_wdata[`CSR_TLBELO1_D];
        csr_tlbelo1_plv <= tlbelo1_wdata[`CSR_TLBELO1_PLV];
        csr_tlbelo1_mat <= tlbelo1_wdata[`CSR_TLBELO1_MAT];
        csr_tlbelo1_g <= tlbelo1_wdata[`CSR_TLBELO1_G];
        csr_tlbelo1_ppn <= tlbelo1_wdata[`CSR_TLBELO1_PPN];
    end
    else if (csr_we && csr_waddr==`CSR_TLBELO1) begin
        csr_tlbelo1_v <= csr_wmask[`CSR_TLBELO1_V] & csr_wdata[`CSR_TLBELO1_V]
                      | ~csr_wmask[`CSR_TLBELO1_V] & csr_tlbelo1_v;
        csr_tlbelo1_d <= csr_wmask[`CSR_TLBELO1_D] & csr_wdata[`CSR_TLBELO1_D]
                      | ~csr_wmask[`CSR_TLBELO1_D] & csr_tlbelo1_d;
        csr_tlbelo1_plv <= csr_wmask[`CSR_TLBELO1_PLV] & csr_wdata[`CSR_TLBELO1_PLV]
                        | ~csr_wmask[`CSR_TLBELO1_PLV] & csr_tlbelo1_plv;
        csr_tlbelo1_mat <= csr_wmask[`CSR_TLBELO1_MAT] & csr_wdata[`CSR_TLBELO1_MAT]
                         | ~csr_wmask[`CSR_TLBELO1_MAT] & csr_tlbelo1_mat;
        csr_tlbelo1_g <= csr_wmask[`CSR_TLBELO1_G] & csr_wdata[`CSR_TLBELO1_G]
                         | ~csr_wmask[`CSR_TLBELO1_G] & csr_tlbelo1_g;
        csr_tlbelo1_ppn <= csr_wmask[`CSR_TLBELO1_PPN] & csr_wdata[`CSR_TLBELO1_PPN]
                         | ~csr_wmask[`CSR_TLBELO1_PPN] & csr_tlbelo1_ppn;
    end
end

// DMW0~1
always @(posedge clk) begin
	if (reset) begin
		csr_dmw0_plv0 <= 1'b0;
		csr_dmw1_plv0 <= 1'b0;
		csr_dmw0_plv3 <= 1'b0;
		csr_dmw1_plv3 <= 1'b0;
	end
	else if (csr_we && csr_waddr == `CSR_DMW0) begin
		csr_dmw0_vseg <= csr_wmask[`CSR_DMW_VSEG] & csr_wdata[`CSR_DMW_VSEG]
					  | ~csr_wmask[`CSR_DMW_VSEG] & csr_dmw0_vseg;
		csr_dmw0_pseg <= csr_wmask[`CSR_DMW_PSEG] & csr_wdata[`CSR_DMW_PSEG]
					  | ~csr_wmask[`CSR_DMW_PSEG] & csr_dmw0_pseg;
		csr_dmw0_mat  <= csr_wmask[`CSR_DMW_MAT]  & csr_wdata[`CSR_DMW_MAT]
					  | ~csr_wmask[`CSR_DMW_MAT]  & csr_dmw0_mat;
		csr_dmw0_plv3 <= csr_wmask[`CSR_DMW_PLV3] & csr_wdata[`CSR_DMW_PLV3]
					  | ~csr_wmask[`CSR_DMW_PLV3] & csr_dmw0_plv3;
		csr_dmw0_plv0 <= csr_wmask[`CSR_DMW_PLV0] & csr_wdata[`CSR_DMW_PLV0]
					  | ~csr_wmask[`CSR_DMW_PLV0] & csr_dmw0_plv0;
	end
	else if (csr_we && csr_waddr == `CSR_DMW1) begin
		csr_dmw1_vseg <= csr_wmask[`CSR_DMW_VSEG] & csr_wdata[`CSR_DMW_VSEG]
					  | ~csr_wmask[`CSR_DMW_VSEG] & csr_dmw1_vseg;
		csr_dmw1_pseg <= csr_wmask[`CSR_DMW_PSEG] & csr_wdata[`CSR_DMW_PSEG]
					  | ~csr_wmask[`CSR_DMW_PSEG] & csr_dmw1_pseg;
		csr_dmw1_mat  <= csr_wmask[`CSR_DMW_MAT]  & csr_wdata[`CSR_DMW_MAT]
					  | ~csr_wmask[`CSR_DMW_MAT]  & csr_dmw1_mat;
		csr_dmw1_plv3 <= csr_wmask[`CSR_DMW_PLV3] & csr_wdata[`CSR_DMW_PLV3]
					  | ~csr_wmask[`CSR_DMW_PLV3] & csr_dmw1_plv3;
		csr_dmw1_plv0 <= csr_wmask[`CSR_DMW_PLV0] & csr_wdata[`CSR_DMW_PLV0]
					  | ~csr_wmask[`CSR_DMW_PLV0] & csr_dmw1_plv0;
	end
end

// Read value
wire [31:0] csr_crmd_value = {23'b0, csr_crmd_datm, csr_crmd_datf, csr_crmd_pg, csr_crmd_da, csr_crmd_ie, csr_crmd_plv};
wire [31:0] csr_prmd_value = {29'b0, csr_prmd_pie, csr_prmd_pplv};
wire [31:0] csr_ecfg_value = {19'b0, csr_ecfg_lie};
wire [31:0] csr_estat_value = {1'b0, csr_estat_esubcode, csr_estat_ecode, 3'b0, csr_estat_is};
wire [31:0] csr_era_value = csr_era_pc;
wire [31:0] csr_badv_value = csr_badv_vaddr;
wire [31:0] csr_eentry_value = {csr_eentry_va, 6'b0};
wire [31:0] csr_save0_value = csr_save0_data;
wire [31:0] csr_save1_value = csr_save1_data;
wire [31:0] csr_save2_value = csr_save2_data;
wire [31:0] csr_save3_value = csr_save3_data;
wire [31:0] csr_tid_value = csr_tid_tid;
wire [31:0] csr_tcfg_value = {csr_tcfg_initval, csr_tcfg_periodic, csr_tcfg_en};
wire [31:0] csr_tval_value = csr_tval;
wire [31:0] csr_ticlr_value = {31'b0, csr_ticlr_clr};
wire [31:0] csr_tlbidx_value = {csr_tlbidx_ne, 1'b0, csr_tlbidx_ps, 20'b0, csr_tlbidx_index};
wire [31:0] csr_tlbehi_value = {csr_tlbehi_vppn, 13'b0};
wire [31:0] csr_tlbelo0_value = {4'b0, csr_tlbelo0_ppn, 1'b0, csr_tlbelo0_g, csr_tlbelo0_mat, csr_tlbelo0_plv, csr_tlbelo0_d, csr_tlbelo0_v};
wire [31:0] csr_tlbelo1_value = {4'b0, csr_tlbelo1_ppn, 1'b0, csr_tlbelo1_g, csr_tlbelo1_mat, csr_tlbelo1_plv, csr_tlbelo1_d, csr_tlbelo1_v};
wire [31:0] csr_asid_value = {8'b0, csr_asid_asidbits, 6'b0, csr_asid_asid};
wire [31:0] csr_tlbrentry_value = {csr_tlbrentry_pa, 6'b0};
wire [31:0] csr_dmw0_value = {csr_dmw0_vseg, 1'b0, csr_dmw0_pseg, 19'b0, csr_dmw0_mat, csr_dmw0_plv3, 2'b0, csr_dmw0_plv0};
wire [31:0] csr_dmw1_value = {csr_dmw1_vseg, 1'b0, csr_dmw1_pseg, 19'b0, csr_dmw1_mat, csr_dmw1_plv3, 2'b0, csr_dmw1_plv0};

assign csr_rdata = {32{csr_raddr==`CSR_CRMD}} & csr_crmd_value
                  | {32{csr_raddr==`CSR_PRMD}} & csr_prmd_value
                  | {32{csr_raddr==`CSR_ECFG}} & csr_ecfg_value
                  | {32{csr_raddr==`CSR_ESTAT}} & csr_estat_value
                  | {32{csr_raddr==`CSR_ERA}} & csr_era_value
                  | {32{csr_raddr==`CSR_BADV}} & csr_badv_value
                  | {32{csr_raddr==`CSR_EENTRY}} & csr_eentry_value
                  | {32{csr_raddr==`CSR_SAVE0}} & csr_save0_value
                  | {32{csr_raddr==`CSR_SAVE1}} & csr_save1_value
                  | {32{csr_raddr==`CSR_SAVE2}} & csr_save2_value
                  | {32{csr_raddr==`CSR_SAVE3}} & csr_save3_value
                  | {32{csr_raddr==`CSR_TID}} & csr_tid_value
                  | {32{csr_raddr==`CSR_TCFG}} & csr_tcfg_value
                  | {32{csr_raddr==`CSR_TVAL}} & csr_tval_value
                  | {32{csr_raddr==`CSR_TICLR}} & csr_ticlr_value
                  | {32{csr_raddr==`CSR_TLBIDX}} & csr_tlbidx_value
                  | {32{csr_raddr==`CSR_TLBEHI}} & csr_tlbehi_value
                  | {32{csr_raddr==`CSR_TLBELO0}} & csr_tlbelo0_value
                  | {32{csr_raddr==`CSR_TLBELO1}} & csr_tlbelo1_value
                  | {32{csr_raddr==`CSR_ASID}} & csr_asid_value
                  | {32{csr_raddr==`CSR_TLBRENTRY}} & csr_tlbrentry_value
                  | {32{csr_raddr==`CSR_DMW0}} & csr_dmw0_value
                  | {32{csr_raddr==`CSR_DMW1}} & csr_dmw1_value;

// Interrupt
assign has_int = ((csr_estat_is[12:0] & csr_ecfg_lie[12:0]) != 13'b0)
              && (csr_crmd_ie == 1'b1);

// Exception entry
assign ex_entry = {csr_eentry_va, 6'b0};
assign tlbr_entry = {csr_tlbrentry_pa, 6'b0};


// TLB needed
assign estat_ecode = csr_estat_ecode;
assign asid_asid = csr_asid_asid;
assign tlbehi_vppn = csr_tlbehi_vppn;
assign tlbidx_index = csr_tlbidx_index;
assign tlbidx_ps = csr_tlbidx_ps;
assign tlbidx_ne = csr_tlbidx_ne;
assign tlbelo0_rdata = csr_tlbelo0_value;
assign tlbelo1_rdata = csr_tlbelo1_value;
assign crmd_pg = csr_crmd_pg;
assign crmd_plv = csr_crmd_plv;
assign crmd_datm = csr_crmd_datm;
assign dmw0 = csr_dmw0_value;
assign dmw1 = csr_dmw1_value;
endmodule
