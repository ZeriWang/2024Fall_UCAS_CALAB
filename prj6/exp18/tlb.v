module tlb
#(
parameter TLBNUM = 16
)
(
input wire clk,

// search port 0 (for fetch)
input wire [ 18:0] s0_vppn, //虚拟页号
input wire s0_va_bit12, //虚拟地址的第12位
input wire [ 9:0] s0_asid, //地址空间标识
output wire s0_found, //是否找到
output wire [$clog2(TLBNUM)-1:0] s0_index, //索引
output wire [ 19:0] s0_ppn, //物理页号
output wire [ 5:0] s0_ps, //页大小
output wire [ 1:0] s0_plv, //权限级别
output wire [ 1:0] s0_mat, //匹配类型
output wire s0_d, //脏位
output wire s0_v, //有效位

// search port 1 (for load/store)
input wire [ 18:0] s1_vppn, //虚拟页号
input wire s1_va_bit12, //虚拟地址的第12位 
input wire [ 9:0] s1_asid, //地址空间标识
output wire s1_found, //是否找到
output wire [$clog2(TLBNUM)-1:0] s1_index, //索引
output wire [ 19:0] s1_ppn, //物理页号
output wire [ 5:0] s1_ps, //页大小
output wire [ 1:0] s1_plv, //权限级别 
output wire [ 1:0] s1_mat, //匹配类型
output wire s1_d, //脏位
output wire s1_v, //有效位

// invtlb opcode
input wire invtlb_valid, //无效化操作有效信号
input wire [ 4:0] invtlb_op, //无效化操作码

// write port
input wire we, //w(rite) e(nable) //写使能
input wire [$clog2(TLBNUM)-1:0] w_index, //索引
input wire w_e, //有效位
input wire [ 18:0] w_vppn, //虚拟页号
input wire [ 5:0] w_ps, //页大小
input wire [ 9:0] w_asid, //地址空间标识
input wire w_g, //全局位，指示该条目是否对所有进程有效
input wire [ 19:0] w_ppn0, //物理页号
input wire [ 1:0] w_plv0, //权限级别 
input wire [ 1:0] w_mat0, //匹配类型
input wire w_d0, //脏位
input wire w_v0, //有效位
input wire [ 19:0] w_ppn1, //物理页号
input wire [ 1:0] w_plv1, //权限级别
input wire [ 1:0] w_mat1, //匹配类型
input wire w_d1, //脏位
input wire w_v1, //有效位

// read port
input wire [$clog2(TLBNUM)-1:0] r_index, //索引
output wire r_e, //有效位
output wire [ 18:0] r_vppn, //虚拟页号
output wire [ 5:0] r_ps, //页大小
output wire [ 9:0] r_asid, //地址空间标识
output wire r_g, //全局位
output wire [ 19:0] r_ppn0, //物理页号
output wire [ 1:0] r_plv0, //权限级别
output wire [ 1:0] r_mat0, //匹配类型
output wire r_d0, //脏位
output wire r_v0, //有效位
output wire [ 19:0] r_ppn1, //物理页号
output wire [ 1:0] r_plv1, //权限级别
output wire [ 1:0] r_mat1, //匹配类型 
output wire r_d1, //脏位
output wire r_v1  //有效位
);

reg [TLBNUM-1:0] tlb_e;
reg [ 18:0] tlb_vppn [TLBNUM-1:0];
reg [ 5:0] tlb_ps [TLBNUM-1:0];
reg [ 9:0] tlb_asid [TLBNUM-1:0];
reg tlb_g [TLBNUM-1:0];
reg [ 19:0] tlb_ppn0 [TLBNUM-1:0];
reg [ 1:0] tlb_plv0 [TLBNUM-1:0];
reg [ 1:0] tlb_mat0 [TLBNUM-1:0];
reg tlb_d0 [TLBNUM-1:0];
reg tlb_v0 [TLBNUM-1:0];
reg [ 19:0] tlb_ppn1 [TLBNUM-1:0];
reg [ 1:0] tlb_plv1 [TLBNUM-1:0];
reg [ 1:0] tlb_mat1 [TLBNUM-1:0];
reg tlb_d1 [TLBNUM-1:0];
reg tlb_v1 [TLBNUM-1:0];

wire tlb_ps4MB [TLBNUM-1:0]; //pagesize 1:4MB, 0:4KB
assign tlb_ps4MB[0] = tlb_ps[0] == 6'b010101;
assign tlb_ps4MB[1] = tlb_ps[1] == 6'b010101;
assign tlb_ps4MB[2] = tlb_ps[2] == 6'b010101;
assign tlb_ps4MB[3] = tlb_ps[3] == 6'b010101;
assign tlb_ps4MB[4] = tlb_ps[4] == 6'b010101;
assign tlb_ps4MB[5] = tlb_ps[5] == 6'b010101;
assign tlb_ps4MB[6] = tlb_ps[6] == 6'b010101;
assign tlb_ps4MB[7] = tlb_ps[7] == 6'b010101;
assign tlb_ps4MB[8] = tlb_ps[8] == 6'b010101;
assign tlb_ps4MB[9] = tlb_ps[9] == 6'b010101;
assign tlb_ps4MB[10] = tlb_ps[10] == 6'b010101;
assign tlb_ps4MB[11] = tlb_ps[11] == 6'b010101;
assign tlb_ps4MB[12] = tlb_ps[12] == 6'b010101;
assign tlb_ps4MB[13] = tlb_ps[13] == 6'b010101;
assign tlb_ps4MB[14] = tlb_ps[14] == 6'b010101;
assign tlb_ps4MB[15] = tlb_ps[15] == 6'b010101;
assign tlb_ps4MB[16] = tlb_ps[16] == 6'b010101;

wire [15:0] match0, match1; //两个接口匹配结果

assign match0[ 0] = (s0_vppn[18:10]==tlb_vppn[ 0][18:10])
                    && (tlb_ps4MB[ 0] || s0_vppn[9:0]==tlb_vppn[ 0][9:0])
                    && ((s0_asid==tlb_asid[ 0]) || tlb_g[ 0]);
assign match0[ 1] = (s0_vppn[18:10]==tlb_vppn[ 1][18:10])
                    && (tlb_ps4MB[ 1] || s0_vppn[9:0]==tlb_vppn[ 1][9:0])
                    && ((s0_asid==tlb_asid[ 1]) || tlb_g[ 1]);
assign match0[ 2] = (s0_vppn[18:10]==tlb_vppn[ 2][18:10])
                    && (tlb_ps4MB[ 2] || s0_vppn[9:0]==tlb_vppn[ 2][9:0])
                    && ((s0_asid==tlb_asid[ 2]) || tlb_g[ 2]);
assign match0[ 3] = (s0_vppn[18:10]==tlb_vppn[ 3][18:10])
                    && (tlb_ps4MB[ 3] || s0_vppn[9:0]==tlb_vppn[ 3][9:0])
                    && ((s0_asid==tlb_asid[ 3]) || tlb_g[ 3]);
assign match0[ 4] = (s0_vppn[18:10]==tlb_vppn[ 4][18:10])
                    && (tlb_ps4MB[ 4] || s0_vppn[9:0]==tlb_vppn[ 4][9:0])
                    && ((s0_asid==tlb_asid[ 4]) || tlb_g[ 4]);
assign match0[ 5] = (s0_vppn[18:10]==tlb_vppn[ 5][18:10])
                    && (tlb_ps4MB[ 5] || s0_vppn[9:0]==tlb_vppn[ 5][9:0])
                    && ((s0_asid==tlb_asid[ 5]) || tlb_g[ 5]);
assign match0[ 6] = (s0_vppn[18:10]==tlb_vppn[ 6][18:10])
                    && (tlb_ps4MB[ 6] || s0_vppn[9:0]==tlb_vppn[ 6][9:0])
                    && ((s0_asid==tlb_asid[ 6]) || tlb_g[ 6]);
assign match0[ 7] = (s0_vppn[18:10]==tlb_vppn[ 7][18:10])
                    && (tlb_ps4MB[ 7] || s0_vppn[9:0]==tlb_vppn[ 7][9:0])
                    && ((s0_asid==tlb_asid[ 7]) || tlb_g[ 7]);
assign match0[ 8] = (s0_vppn[18:10]==tlb_vppn[ 8][18:10])
                    && (tlb_ps4MB[ 8] || s0_vppn[9:0]==tlb_vppn[ 8][9:0])
                    && ((s0_asid==tlb_asid[ 8]) || tlb_g[ 8]);
assign match0[ 9] = (s0_vppn[18:10]==tlb_vppn[ 9][18:10])
                    && (tlb_ps4MB[ 9] || s0_vppn[9:0]==tlb_vppn[ 9][9:0])
                    && ((s0_asid==tlb_asid[ 9]) || tlb_g[ 9]);
assign match0[10] = (s0_vppn[18:10]==tlb_vppn[10][18:10])
                    && (tlb_ps4MB[10] || s0_vppn[9:0]==tlb_vppn[10][9:0])
                    && ((s0_asid==tlb_asid[10]) || tlb_g[10]);
assign match0[11] = (s0_vppn[18:10]==tlb_vppn[11][18:10])
                    && (tlb_ps4MB[11] || s0_vppn[9:0]==tlb_vppn[11][9:0])
                    && ((s0_asid==tlb_asid[11]) || tlb_g[11]);
assign match0[12] = (s0_vppn[18:10]==tlb_vppn[12][18:10])
                    && (tlb_ps4MB[12] || s0_vppn[9:0]==tlb_vppn[12][9:0])
                    && ((s0_asid==tlb_asid[12]) || tlb_g[12]);
assign match0[13] = (s0_vppn[18:10]==tlb_vppn[13][18:10])
                    && (tlb_ps4MB[13] || s0_vppn[9:0]==tlb_vppn[13][9:0])
                    && ((s0_asid==tlb_asid[13]) || tlb_g[13]);
assign match0[14] = (s0_vppn[18:10]==tlb_vppn[14][18:10])
                    && (tlb_ps4MB[14] || s0_vppn[9:0]==tlb_vppn[14][9:0])
                    && ((s0_asid==tlb_asid[14]) || tlb_g[14]);
assign match0[15] = (s0_vppn[18:10]==tlb_vppn[15][18:10])
                    && (tlb_ps4MB[15] || s0_vppn[9:0]==tlb_vppn[15][9:0])
                    && ((s0_asid==tlb_asid[15]) || tlb_g[15]);
assign match1[ 0] = (s1_vppn[18:10]==tlb_vppn[ 0][18:10])
                    && (tlb_ps4MB[ 0] || s1_vppn[9:0]==tlb_vppn[ 0][9:0])
                    && ((s1_asid==tlb_asid[ 0]) || tlb_g[ 0]);
assign match1[ 1] = (s1_vppn[18:10]==tlb_vppn[ 1][18:10])
                    && (tlb_ps4MB[ 1] || s1_vppn[9:0]==tlb_vppn[ 1][9:0])
                    && ((s1_asid==tlb_asid[ 1]) || tlb_g[ 1]);
assign match1[ 2] = (s1_vppn[18:10]==tlb_vppn[ 2][18:10])
                    && (tlb_ps4MB[ 2] || s1_vppn[9:0]==tlb_vppn[ 2][9:0])
                    && ((s1_asid==tlb_asid[ 2]) || tlb_g[ 2]);
assign match1[ 3] = (s1_vppn[18:10]==tlb_vppn[ 3][18:10])
                    && (tlb_ps4MB[ 3] || s1_vppn[9:0]==tlb_vppn[ 3][9:0])
                    && ((s1_asid==tlb_asid[ 3]) || tlb_g[ 3]);
assign match1[ 4] = (s1_vppn[18:10]==tlb_vppn[ 4][18:10])
                    && (tlb_ps4MB[ 4] || s1_vppn[9:0]==tlb_vppn[ 4][9:0])
                    && ((s1_asid==tlb_asid[ 4]) || tlb_g[ 4]);
assign match1[ 5] = (s1_vppn[18:10]==tlb_vppn[ 5][18:10])
                    && (tlb_ps4MB[ 5] || s1_vppn[9:0]==tlb_vppn[ 5][9:0])
                    && ((s1_asid==tlb_asid[ 5]) || tlb_g[ 5]);
assign match1[ 6] = (s1_vppn[18:10]==tlb_vppn[ 6][18:10])
                    && (tlb_ps4MB[ 6] || s1_vppn[9:0]==tlb_vppn[ 6][9:0])
                    && ((s1_asid==tlb_asid[ 6]) || tlb_g[ 6]);
assign match1[ 7] = (s1_vppn[18:10]==tlb_vppn[ 7][18:10])
                    && (tlb_ps4MB[ 7] || s1_vppn[9:0]==tlb_vppn[ 7][9:0])
                    && ((s1_asid==tlb_asid[ 7]) || tlb_g[ 7]);
assign match1[ 8] = (s1_vppn[18:10]==tlb_vppn[ 8][18:10])
                    && (tlb_ps4MB[ 8] || s1_vppn[9:0]==tlb_vppn[ 8][9:0])
                    && ((s1_asid==tlb_asid[ 8]) || tlb_g[ 8]);
assign match1[ 9] = (s1_vppn[18:10]==tlb_vppn[ 9][18:10])
                    && (tlb_ps4MB[ 9] || s1_vppn[9:0]==tlb_vppn[ 9][9:0])
                    && ((s1_asid==tlb_asid[ 9]) || tlb_g[ 9]);
assign match1[10] = (s1_vppn[18:10]==tlb_vppn[10][18:10])
                    && (tlb_ps4MB[10] || s1_vppn[9:0]==tlb_vppn[10][9:0])
                    && ((s1_asid==tlb_asid[10]) || tlb_g[10]);
assign match1[11] = (s1_vppn[18:10]==tlb_vppn[11][18:10])
                    && (tlb_ps4MB[11] || s1_vppn[9:0]==tlb_vppn[11][9:0])
                    && ((s1_asid==tlb_asid[11]) || tlb_g[11]);
assign match1[12] = (s1_vppn[18:10]==tlb_vppn[12][18:10])
                    && (tlb_ps4MB[12] || s1_vppn[9:0]==tlb_vppn[12][9:0])
                    && ((s1_asid==tlb_asid[12]) || tlb_g[12]);
assign match1[13] = (s1_vppn[18:10]==tlb_vppn[13][18:10])
                    && (tlb_ps4MB[13] || s1_vppn[9:0]==tlb_vppn[13][9:0])
                    && ((s1_asid==tlb_asid[13]) || tlb_g[13]);
assign match1[14] = (s1_vppn[18:10]==tlb_vppn[14][18:10])
                    && (tlb_ps4MB[14] || s1_vppn[9:0]==tlb_vppn[14][9:0])
                    && ((s1_asid==tlb_asid[14]) || tlb_g[14]);
assign match1[15] = (s1_vppn[18:10]==tlb_vppn[15][18:10])
                    && (tlb_ps4MB[15] || s1_vppn[9:0]==tlb_vppn[15][9:0])
                    && ((s1_asid==tlb_asid[15]) || tlb_g[15]);

//查找结果
assign s0_found = |match0;
assign s1_found = |match1;

//用编码器取得索引
encoder pe0(
    .in(match0),
    .out(s0_index)
);
encoder pe1(
    .in(match1),
    .out(s1_index)
);

//将TLB各项对应信息填入查找端口
assign s0_ppn = ~tlb_ps4MB[s0_index] & s0_va_bit12 | tlb_ps4MB[s0_index] & s0_vppn[8] ? tlb_ppn1[s0_index] : tlb_ppn0[s0_index];
assign s0_ps = tlb_ps[s0_index];
assign s0_plv = ~tlb_ps4MB[s0_index] & s0_va_bit12 | tlb_ps4MB[s0_index] & s0_vppn[8] ? tlb_plv1[s0_index] : tlb_plv0[s0_index];
assign s0_mat = ~tlb_ps4MB[s0_index] & s0_va_bit12 | tlb_ps4MB[s0_index] & s0_vppn[8] ? tlb_mat1[s0_index] : tlb_mat0[s0_index];
assign s0_d = ~tlb_ps4MB[s0_index] & s0_va_bit12 | tlb_ps4MB[s0_index] & s0_vppn[8] ? tlb_d1[s0_index] : tlb_d0[s0_index];
assign s0_v = ~tlb_ps4MB[s0_index] & s0_va_bit12 | tlb_ps4MB[s0_index] & s0_vppn[8] ? tlb_v1[s0_index] : tlb_v0[s0_index];
assign s1_ppn = ~tlb_ps4MB[s1_index] & s1_va_bit12 | tlb_ps4MB[s1_index] & s1_vppn[8] ? tlb_ppn1[s1_index] : tlb_ppn0[s1_index];
assign s1_ps = tlb_ps[s1_index];
assign s1_plv = ~tlb_ps4MB[s1_index] & s1_va_bit12 | tlb_ps4MB[s1_index] & s1_vppn[8] ? tlb_plv1[s1_index] : tlb_plv0[s1_index];
assign s1_mat = ~tlb_ps4MB[s1_index] & s1_va_bit12 | tlb_ps4MB[s1_index] & s1_vppn[8] ? tlb_mat1[s1_index] : tlb_mat0[s1_index];
assign s1_d = ~tlb_ps4MB[s1_index] & s1_va_bit12 | tlb_ps4MB[s1_index] & s1_vppn[8] ? tlb_d1[s1_index] : tlb_d0[s1_index];
assign s1_v = ~tlb_ps4MB[s1_index] & s1_va_bit12 | tlb_ps4MB[s1_index] & s1_vppn[8] ? tlb_v1[s1_index] : tlb_v0[s1_index];

//写操作
always @(posedge clk) begin
    if (we) begin
        tlb_e[w_index] <= w_e;
        tlb_vppn[w_index] <= w_vppn;
        tlb_ps[w_index] <= w_ps;
        tlb_asid[w_index] <= w_asid;
        tlb_g[w_index] <= w_g;
        tlb_ppn0[w_index] <= w_ppn0;
        tlb_plv0[w_index] <= w_plv0;
        tlb_mat0[w_index] <= w_mat0;
        tlb_d0[w_index] <= w_d0;
        tlb_v0[w_index] <= w_v0;
        tlb_ppn1[w_index] <= w_ppn1;
        tlb_plv1[w_index] <= w_plv1;
        tlb_mat1[w_index] <= w_mat1;
        tlb_d1[w_index] <= w_d1;
        tlb_v1[w_index] <= w_v1;
    end
end

//读操作
assign r_e = tlb_e[r_index];
assign r_vppn = tlb_vppn[r_index];
assign r_ps = tlb_ps[r_index];
assign r_asid = tlb_asid[r_index];
assign r_g = tlb_g[r_index];
assign r_ppn0 = tlb_ppn0[r_index];
assign r_plv0 = tlb_plv0[r_index];
assign r_mat0 = tlb_mat0[r_index];
assign r_d0 = tlb_d0[r_index];
assign r_v0 = tlb_v0[r_index];
assign r_ppn1 = tlb_ppn1[r_index];
assign r_plv1 = tlb_plv1[r_index];
assign r_mat1 = tlb_mat1[r_index];
assign r_d1 = tlb_d1[r_index];
assign r_v1 = tlb_v1[r_index];

//无效化操作
genvar i;
generate
    for (i = 0; i < TLBNUM; i = i + 1) begin: invtlb
        always @(posedge clk) begin
            if (invtlb_valid) begin
                if (invtlb_op == 5'h0 || invtlb_op == 5'h1) begin
                    tlb_e[i] <= 1'b0;
                end
                else if (invtlb_op == 5'h2 && tlb_g[i]) begin
                    tlb_e[i] <= 1'b0;
                end
                else if (invtlb_op == 5'h3 && !tlb_g[i]) begin
                    tlb_e[i] <= 1'b0;
                end
                else if (invtlb_op == 5'h4 && tlb_asid[i] == s1_asid) begin
                    tlb_e[i] <= 1'b0;
                end
                else if (invtlb_op == 5'h5 && !tlb_g[i] && tlb_asid[i] == s1_asid && tlb_vppn[i] == s1_vppn) begin
                    tlb_e[i] <= 1'b0;
                end
                else if (invtlb_op == 5'h6 && (tlb_g[i] || tlb_asid[i] == s1_asid) && tlb_vppn[i] == s1_vppn) begin
                    tlb_e[i] <= 1'b0;
                end
            end
        end
    end
endgenerate

endmodule