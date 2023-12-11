###################################################################################################################################

This project is developed by CS23M005(Patha Yedukondalu) and CS23S023(Samyuktha M)
Project Title: Pipelined Double Precision Floating Point Multiplier

###################################################################################################################################

Project Structure:
This project consist of the following files arranged as follows:
1. The 'src' folder contains the complete source code
   (a) TbMul.bsv : complete code (multiplier module along with test bench in the same file)
   (b) mkMulPipe.v : verilog version(generated using BDW) of multiplier module
   The header files used are also provided in the folder:
   (c) FloatingPoint.bo : Library imported for floating point representation
   The files obtained when the 'TbMul.bsv' is compiled and run are
   (d) TbMul.bo
   (e) MkTbMul.ba
   (f) MkMulPipe.ba

2. The 'results' folder contains the results of the implementation
   (a) synth1.v : generated using yosys tool after synthesis
   (b) netlist.v : netlist for multiplier module (mkMulPipe.v) generated using yosys
   (c) chip_area.txt : chip area calculations output file generated using yosys

3. Detailed_Documentation.pdf : eloberates literature survey, approach & hardware structure along with testing and results


###################################################################################################################################

Design Decisions:
1. hardware is developed as a 5 stage pipeline
	stage 1 : parsing the input, result_exp(sign & value) evaluation and partial products generation(52/6 ~ 9 partial products)
			   flags check for Zero, inf, Nan
	stage 2 : addition of 9 partial products parallelly (3 at a time)
	stage 3 : result_mantisa calculation of result by adding above 3 summations
	stage 4 : Denormalization of result_mantisa
	stage 5 : flags check for OVF, UNF, Subnormal and rounding off (keeping MSb 52 bits only, chopping off remaining bits)
2. Multiplier interface: 
	-> send method for inputting two 64 bit numbers to the multiplier
	-> receive method for receiving the multiplication result and flags
3. 13 bits are used for storing the result_exp to capture ovf, unf and subnormal
4. 108 bits are used for storing the result_mantisa related calculations
5. 11 bits, 52 bits (final exp, final mantisa) are derived from above calculations as final outputs
6. sign, exp, mantisa along with 6 flags(zero, inf, nan, ovf, unf, subnormal) are given as output

###################################################################################################################################

Verification Methodology:
1. All possible combinations of multiplications were given as test cases to the multiplier module and verified
2. multiplication combinations such as (prioritized as below)
	-> Normal 64 bit number * norma 64 bit number
	-> One/both of the input is Nan
	-> One/both of the input is Zero
	-> One/both of the input is inf
	-> Inputs that result in Overflow after multiplication
	-> Inputs that result in underflow after multiplication
	-> Inputs that result in subnormal number as output
3. Each test case is explicitily written as a seperate rule
	-> Test case rules are generated using C

4. Understanding the results:
	-> Output appears after 5 clock pulses
	-> Result should be considered along with flags
		-> flag Zero, flag Nan, flag OVF, flag UNF, flag Subnormal, flag Inf
		
###################################################################################################################################

Running the code:
1. Provide the input numbers as hexadecimal values in rl_sendX of method mkTbMul in TbMul.bsv
2. Click on the 'Compile and run' option for the testbench 'TbMul.bsv' in the BDW tool.
3. Observe the result after 5 clockpulses

###################################################################################################################################
