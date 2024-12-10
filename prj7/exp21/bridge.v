module bridge(
    // axi4-lite interface
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
    output wire [ 3:0] wid,
    output wire [31:0] wdata,
    output wire [ 3:0] wstrb,
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
    input  wire [31:0] data_sram_wdata,
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
    input  wire        inst_sram_using
);

assign arid = (!memory_access | (memory_access && (data_write_ok | data_rdata_ok)) | inst_sram_using) ? 4'b0000 : 4'b0001; //0指令，1数据
assign araddr = (arid == 4'b0) ? inst_sram_addr : data_sram_addr; //读地址
assign arlen = arid == 4'b0 ? {2{icache_rd_type[2]}} : 8'b00000000; // exp21
assign arsize = (arid == 4'b0) ? inst_sram_size : data_sram_size; //读大小
assign arburst = 2'b01; //固定为01
assign arlock = 2'b00; //固定为0
assign arcache = 4'b0000; //固定为0
assign arprot = 3'b000; //固定为0
assign arvalid = (inst_sram_req) | (data_sram_req & ~data_sram_wr); //读请求有效

assign rready = (data_raddr_ok & !data_rdata_ok) | (inst_raddr_ok & (!memory_access || (memory_access && (data_write_ok || data_rdata_ok))))
                | inst_sram_using & inst_raddr_ok; // 数据读完成握手或指令读完成握手

assign awid = 4'b0001; //固定为1
assign awaddr = data_sram_addr; //写地址
assign awlen = 8'b00000000; //固定为0
assign awsize = data_sram_size; //写大小
assign awburst = 2'b01; //固定为01
assign awlock = 2'b00; //固定为0
assign awcache = 4'b0000; //固定为0
assign awprot = 3'b000; //固定为0
assign awvalid = data_sram_req & data_sram_wr; //写请求有效

assign wid = 4'b0001; //固定为1
assign wdata = data_sram_wdata; //写数据
assign wstrb = data_sram_wstrb; //写掩码
assign wlast = 1'b1; //固定为1
assign wvalid = data_waddr_ok & !data_wdata_ok; //写请求数据有效

assign bready = data_wdata_ok; // 写数据完成二次握手

assign inst_sram_rdata = rdata; //指令码
assign inst_sram_addr_ok = arvalid & arready & (arid == 4'b0); //指令地址有效
assign inst_sram_data_ok = rvalid & rready & inst_raddr_ok & rlast; //指令数据有效

assign data_sram_rdata = {32{arid == 4'b1}} & rdata; //读数据
assign data_sram_addr_ok = arvalid & arready & (arid == 4'b1) & ~data_sram_wr | awvalid & awready & (arid == 4'b1) & data_sram_wr & ~inst_sram_using; //数据地址有效
assign data_sram_data_ok = rvalid & rready & ~data_sram_wr | bvalid & bready & data_sram_wr & ~inst_sram_using; //读写数据有效

endmodule