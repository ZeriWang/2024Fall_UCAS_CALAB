module encoder
#(
    parameter WIDTH = 16
)
(
    input wire [WIDTH-1:0] in,
    output reg [$clog2(WIDTH)-1:0] out
);

    integer i;
    always @(*) begin
        out <= 0;
        for (i = 0; i < WIDTH; i = i + 1) begin
            if (in[i]) begin
                out <= i;
            end
        end
    end

endmodule