module multiplier(
	input  wire [ 2:0] mul_op,
	input  wire [31:0] mul_src1,
	input  wire [31:0] mul_src2,
	output wire [31:0] mul_result
);

wire op_mul_w;   // multiply
wire op_mulh_w;  // multiply high-bits
wire op_mulh_wu; // multiply high-bits unsigned

assign op_mul_w   = mul_op[0];
assign op_mulh_w  = mul_op[1];
assign op_mulh_wu = mul_op[2];

wire [32:0] mul_src1_ext;
wire [32:0] mul_src2_ext;

assign mul_src1_ext = {op_mulh_w & mul_src1[31], mul_src1}; //位数扩展
assign mul_src2_ext = {op_mulh_w & mul_src2[31], mul_src2}; //位数扩展

// 66-bit signed multiply
wire signed [65:0] mul_res_66;
assign mul_res_66 = $signed(mul_src1_ext) * $signed(mul_src2_ext); // 直接调用Xillinx IP实现乘法功能

wire [31:0] mull_result; // low 32 bits
wire [31:0] mulh_result; // high 32 bits
// wire [31:0] mulh_wu_result;
assign mull_result = mul_res_66[31: 0];
assign mulh_result = mul_res_66[63:32];

// final result mux
assign mul_result = ({32{op_mul_w              }} & mull_result) //选取低32位
				  | ({32{op_mulh_w | op_mulh_wu}} & mulh_result) //选取高32位
;

endmodule