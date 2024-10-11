module csr_regfile(
    input  wire        clk,
    input  wire        rst,
    // READ PORT 1
    input  wire [13:0] raddr,
    output wire [31:0] rdata,
    // WRITE PORT
    input  wire [31:0] we,       //write enable, HIGH valid
    input  wire [13:0] waddr,
    input  wire [31:0] wdata,
    input  wire        ertn,
    input  wire        wb_ex,
    input  wire [31:0] wb_pc,
    input  wire [ 5:0] ecode //例外号
);
`define CSR_CRMD waddr==14'b0
`define CSR_PRMD waddr==14'b1
`define CSR_ESTATUS waddr==14'b101
`define CSR_ERA waddr==14'b110
`define CSR_EENTRY waddr==14'b1100
`define CSR_SAVE (waddr==14'b110000 | waddr==14'b110001 | waddr==14'b110010 | waddr==14'b110011)
reg [31:0] csr[51:0];

wire [31:0] crmd;
wire [31:0] prmd;
wire [31:0] estat;
wire [31:0] era;
wire [31:0] eentry;
wire [31:0] save0;
wire [31:0] save1;
wire [31:0] save2;
wire [31:0] save3;

assign crmd = csr[0];
assign prmd = csr[1];
assign estat = csr[5];
assign era = csr[6];
assign eentry = csr[12];
assign save0 = csr[48];
assign save1 = csr[49];
assign save2 = csr[50];
assign save3 = csr[51];

wire csr_find_w;
wire csr_find_r;

//WRITE
always @(posedge clk) begin
    if (rst) begin
        csr[0] <= 32'b1000;
        csr[5] <= 32'b0;
    end
    else if (wb_ex) begin
        csr[1][2:0] <= csr[0][2:0];
        csr[0][2:0] <= 3'b0;
        csr[5][21:16] <= ecode;
        csr[6] <= wb_pc;
    end
    else if (ertn) begin
        csr[0][2:0] <= csr[1][2:0];
    end
    else if (csr_find_w) begin
        csr[waddr] <= ~we & csr[waddr] | we & wdata;
    end
end

assign csr_find_w = |we & ((waddr == 14'b0) | 
                    (waddr == 14'b1) |
                    (waddr == 14'b101) |
                    (waddr == 14'b110) |
                    (waddr == 14'b1100) |
                    (waddr == 14'b110000) |
                    (waddr == 14'b110001) |
                    (waddr == 14'b110010) |
                    (waddr == 14'b110011));

assign csr_find_r = (raddr == 14'b0) |
                    (raddr == 14'b1) |
                    (raddr == 14'b101) |
                    (raddr == 14'b110) |
                    (raddr == 14'b1100) |
                    (raddr == 14'b110000) |
                    (raddr == 14'b110001) |
                    (raddr == 14'b110010) |
                    (raddr == 14'b110011);

//READ OUT 1
assign rdata = (~csr_find_r) ? 32'b0 : csr[ertn ? 32'b110 :raddr];

endmodule
