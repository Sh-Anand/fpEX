# Description
Vector FP32/FP16/BF16 exponentiation module written in Chisel. Fully pipelined. 4 cycle latency end to end
Just copy-paste fpex/ and hardfloat/ into your project to reuse

## Commands
- `./mill compile`
- `./mill run` (generates Verilog in `generated/*.v`)
- `./mill clean`

## Test Targets
- ./mill test : Runs everything
- `./mill test.testOnly fpex.FPEXShortSpec` : Runs all FP32+FP16+BF16
- `./mill test.testOnly fpex.FPEXMediumSpec`
- `./mill test.testOnly fpex.FPEXLongSpec`
- `./mill test.testOnly fpex.FPEXWideSpec`
- `./mill test.testOnly fpex.FPEXShortSpecFP32`
- `./mill test.testOnly fpex.FPEXShortSpecFP16`
- `./mill test.testOnly fpex.FPEXShortSpecBF16`
- `./mill test.testOnly fpex.FPEXMediumSpecFP32`
- `./mill test.testOnly fpex.FPEXMediumSpecFP16`
- `./mill test.testOnly fpex.FPEXMediumSpecBF16`
- `./mill test.testOnly fpex.FPEXLongSpecFP32`
- `./mill test.testOnly fpex.FPEXLongSpecFP16`
- `./mill test.testOnly fpex.FPEXLongSpecBF16`
- `./mill test.testOnly fpex.FPEXWideSpecFP32`
- `./mill test.testOnly fpex.FPEXWideSpecFP16`
- `./mill test.testOnly fpex.FPEXWideSpecBF16`

NOTE: fp32 tests fail for extreme inputs, fp32 accuracy is not great. Up the LUT bits if you need it.
