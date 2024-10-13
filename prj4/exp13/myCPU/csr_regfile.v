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
    input  [ 8:0] wb_esubcode
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


`define CSR_CRMD_PLV 1:0
`define CSR_CRMD_IE 2
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

`define ECODE_ADE 6'h8
`define ECODE_ALE 6'h9
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
reg [1:0]  csr_crmd_plv;
reg        csr_crmd_ie;
wire       csr_crmd_da;
wire       csr_crmd_pg;
wire [2:0] csr_crmd_datf;
wire [2:0] csr_crmd_datm;

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
    if (reset)
        csr_crmd_ie <= 1'b0;
    else if (wb_ex)
        csr_crmd_ie <= 1'b0;
    else if (ertn_flush)
        csr_crmd_ie <= csr_prmd_pie;
    else if (csr_we && csr_waddr==`CSR_CRMD)
        csr_crmd_ie <= csr_wmask[`CSR_CRMD_IE] & csr_wdata[`CSR_CRMD_IE]
                    | ~csr_wmask[`CSR_CRMD_IE] & csr_crmd_ie;
end

assign csr_crmd_da = 1'b1;
assign csr_crmd_pg = 1'b0;
assign csr_crmd_datf = 2'b00;
assign csr_crmd_datm = 2'b00;

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
assign wb_ex_addr_err = wb_ecode==`ECODE_ADE || wb_ecode==`ECODE_ALE;
always @(posedge clk) begin
    if (wb_ex && wb_ex_addr_err)
        csr_badv_vaddr <= (wb_ecode==`ECODE_ADE &&
                           wb_esubcode==`ESUBCODE_ADEF) ? wb_pc : wb_vaddr;
end

// EENTRY
always @(posedge clk) begin
    if (csr_we && csr_waddr==`CSR_EENTRY)
        csr_eentry_va <= csr_wmask[`CSR_EENTRY_VA] & csr_wdata[`CSR_EENTRY_VA]
                      | ~csr_wmask[`CSR_EENTRY_VA] & csr_eentry_va;
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
                  | {32{csr_raddr==`CSR_TICLR}} & csr_ticlr_value;

// Interrupt
assign has_int = ((csr_estat_is[12:0] & csr_ecfg_lie[12:0]) != 13'b0)
              && (csr_crmd_ie == 1'b1);

// Exception entry
assign ex_entry = {csr_eentry_va, 6'b0};
endmodule
