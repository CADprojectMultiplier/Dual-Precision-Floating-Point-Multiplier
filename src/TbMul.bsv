package TbMul;
//imported libraries
import FloatingPoint ::*;
import FIFO::*;

interface Ifc_mul_dp;                                              
	method Action send(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52)) data_in);
	method Tuple4#(Bit#(1),Bit#(11), Bit#(104), Bit#(6)) receive();
endinterface



(* synthesize *)

module mkTbMul (Empty);

	Reg#(int) x <- mkReg ('h10);
	Reg#(int) y1 <- mkReg ('h00);
	Ifc_mul_dp pipe <- mkMulPipe;
	
    Reg#(Bit#(8)) rg_cycle <- mkReg(0);

//Testcases

    rule rl_send0(rg_cycle==0); //sending first set of inputs in first cycle - unf
      FloatingPoint#(11, 52) f,g;

      f.sign = True;
      f.exp = 11'h001;
      f.sfd = 52'h400000000000f;
      
      g.sign = False;
      g.exp = 11'h001;
      g.sfd = 52'h400000000000f;


      pipe.send(tuple2(f,g));
      $display("input cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule
    
    rule rl_send1(rg_cycle==1);   //sending first set of inputs in third cycle - ovf
      FloatingPoint#(11, 52) f,g;

      f.sign = False;
      f.exp = 11'h403;
      f.sfd = 52'h8000000000000;
      
      g.sign = False;
      g.exp = 11'h401;
      g.sfd = 52'h4000000000000;


      pipe.send(tuple2(f,g));
      $display("input cycle:",rg_cycle," ",fshow(f),fshow(g));

    endrule
    
    rule rl_send2(rg_cycle==2); //sending second set of inputs in second cycle - zero
      FloatingPoint#(11, 52) f,g;

      f.sign = False;
      f.exp = 11'h401;
      f.sfd = 52'h4000000000000;
      
      g.sign = False;
      g.exp = 11'h00;
      g.sfd = 52'h0;


      pipe.send(tuple2(f,g));
      $display("input cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule
    
    rule rl_send3(rg_cycle==3); //sending third set of inputs in third cycle - NAN
      FloatingPoint#(11, 52) f,g;

      f.sign = False;
      f.exp = 11'h7ff;
      f.sfd = 52'hC000000000000;
      
      g.sign = False;
      g.exp = 11'h408;
      g.sfd = 52'hC000000000000;


      pipe.send(tuple2(f,g));
      $display("input cycle:",rg_cycle," ",fshow(f),fshow(g));

    endrule
	rule rl_send4(rg_cycle==4); //sending fourth set of inputs in fourth cycle - INF
      FloatingPoint#(11, 52) f,g;

      f.sign = False;
      f.exp = 11'h7FF;
      f.sfd = 52'h0000000000000;
      
      g.sign = False;
      g.exp = 11'h408;
      g.sfd = 52'hC000000000000;


      pipe.send(tuple2(f,g));
      $display("input cycle:",rg_cycle," ",fshow(f),fshow(g));

    endrule
    
    rule rl_send5(rg_cycle==5); //subnormal
      FloatingPoint#(11, 52) f,g;

      f.sign = False;
      f.exp = 11'd1003;
      f.sfd = 52'h8000000000000;
      
      g.sign = False;
      g.exp = 11'd3;
      g.sfd = 52'h4000000000000;


      pipe.send(tuple2(f,g));
      $display("input cycle:",rg_cycle," ",fshow(f),fshow(g));

    endrule
	
    rule rl_receive;
      //Tuple3#(Bit#(1),Bit#(11), Bit#(108)) r;

      let r = pipe.receive();
      
      $display("output cycle:",rg_cycle," flags: %b, ",tpl_4(r), " sign: %h, ",tpl_1(r), " exp: %h, ",tpl_2(r)[10:0], " man: %h, ",tpl_3(r));
      
    endrule

    rule rl_end;
      rg_cycle <= rg_cycle + 1;  //incrementing the clock
      if (rg_cycle > 10) begin
		$finish(0);
      end
    endrule
endmodule: mkTbMul

function Bit#(2) ha(Bit#(1) a, Bit#(1) b);
	return {a&b, a^b};
endfunction	

function Tuple2#(Bit#(1), Bit#(1)) fa(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
	Bit#(2) t = ha(a,b);
	Bit#(1) cout;
	let t2 = ha(t[0], cin);
	cout = t2[1]|t[1];
	return tuple2(cout, t2[0]);
endfunction

function int incr1(int a);
	return a+1;
endfunction


function Tuple2#(Bit#(32), Bit#(32)) partialProd(Bit#(32) a, Bit#(1) bi, Bit#(32) sum, Bit#(32) carry);
	Bit#(32) sumout;
	Bit#(32) cout;
	for (Integer i=0; i<32; i=i+1) begin
		let aa = fa(a[i]&bi, sum[i], carry[i]);
		cout[i] = tpl_1(aa);
		sumout[i] = tpl_2(aa);
	end
	return tuple2(cout,sumout);		
endfunction

function Bit#(13) eval_exp(Bit#(11) e1, Bit#(11) e2); 

	 Bit#(13) er= zeroExtend(e1)+zeroExtend(e2)+negate(13'd1023); 
	 
	 return er;
	 
endfunction

function Bit#(108) fn_gen_pp_dp(Bit#(53) m, Bit#(6) b);                    //generates the partial products for mantissa multiplication
	 Bit#(60) y=0;
	 Bit#(60) a=zeroExtend(m);
	 Bit#(60) res1=0;
	 Bit#(60) res2=0;
	 Bit#(60) res3=0;
	 Bit#(60) res4=0;
	 Bit#(60) res6=0;
	 Bit#(60) res5=0;
	 res1 = (b[0]==1'b1)?(a):60'd0;
	 res2 =  (b[1]==1'b1)?(a<<1):60'd0;
	 res3 =  (b[2]==1'b1)?(a<<2):60'd0;
	 res4 =  (b[3]==1'b1)?(a<<3):60'd0;
	 res5 =  (b[4]==1'b1)?(a<<4):60'd0;
	 res6 =  (b[5]==1'b1)?(a<<5):60'd0;
	 y =  res1+res2+res3+res4+res5+res6;
	 return zeroExtend(y);          // extending the result
endfunction
/*
function Bit#(108) fn_add_dp4(Bit#(108) x1,Bit#(108) x2,Bit#(108) x3,Bit#(108) x4, Bit#(108) x5,Bit#(108) x6,Bit#(108) x7,Bit#(108) x8, Bit#(108) x9);
	 let x10 = x1+x2+x3+x4+x5+x6+x7+x8+x9;             //generating product from partial products
	 return x10;
endfunction
*/
function Bit#(108) fn_add_dp3(Bit#(108) x1,Bit#(108) x2,Bit#(108) x3);
	 let x5 = x1+x2+x3;             //generating product from partial products
	 return x5;
endfunction

function Bit#(6) fn_gen_flags(Bit#(11) e1, Bit#(52) m1, Bit#(11) e2, Bit#(52) m2);
// 5-flags "overflow, underflow, nan, inf, zero"
	Bit#(6) flags = 6'd0;
	flags[0] = (((|e1==1'b0)&&(|m1==1'b0))||((|e2==1'b0)&&(|m2==1'b0)))?1'b1:1'b0; // if any of the input is zero
	flags[1] = (((&e1==1'b1)&&(|m1==1'b0))||((&e2==1'b1)&&(|m2==1'b0)))?1'b1:1'b0; // if any of the input is inf 	
	flags[2] = (((&e1==1'b1)&&(|m1==1'b1))||((&e2==1'b1)&&(|m2==1'b1)))?1'b1:1'b0; // if any of the input is nan - update for zero-inf, inf-zero case aswell
	flags[3] = 0; // this flag will be updated after the multiplication is performed
	flags[4] = 0; // this flag will be updated after the multiplication is performed	
	flags[5] = 0; // represents subnormal number
	return flags;	
endfunction



(* synthesize *)

module mkMulPipe (Ifc_mul_dp);
	
	Reg#(Bit#(13)) rg_er<-mkReg(0);
	Reg#(Bit#(1)) rg_sr<-mkReg(0);
	
	
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy1 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy2 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy3 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy4 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy5 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy6 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy7 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy8 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy9 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy10 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy11 <- mkReg(tuple2(unpack(0),unpack(0)));
	
	
	
	rule rl_eval_exponent_sign_stage1;                                         
	  match {.opA, .opB} = d_copy1;
	  rg_er <= eval_exp(opA.exp,opB.exp);                   //evaluating exponent, 1st stage
	  rg_sr <= pack(opA.sign != opB.sign);                                         //evaluating sign, 1st stage
	  /*$display("Stage1:");
	  $display("e1_er: %h ", opA.exp);
	  $display("e2_er: %h ", opB.exp);*/
	  //$display("ng_er: %h ", negate(12'd1023));
	  
	endrule
	
	Reg#(Bit#(108)) rg_partial_product0_1 <- mkReg(0); // size 60 or 59
	Reg#(Bit#(108)) rg_partial_product1_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product2_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product3_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product4_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product5_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product6_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product7_1 <- mkReg(0);
	Reg#(Bit#(108)) rg_partial_product8_1 <- mkReg(0);
	
	Reg#(Bit#(6)) rg_flags_s1 <-mkReg(0);
	
	
	rule rl_eval_partial_product_1_stage1;                  
	  match {.opA, .opB} = d_copy2;                                  
	  Bit#(11) expA1 = 0;
	  Bit#(11) expB1 = 0;
	  Bit#(52) sfdA1 = 0;
	  Bit#(52) sfdB1 = 0;
	  
	  sfdA1 = opA.sfd;
	  expA1 = opA.exp;
	  sfdB1 = opB.sfd;
	  expB1 = opB.exp;
	  
	  rg_flags_s1 <= fn_gen_flags(expA1, sfdA1, expB1, sfdB1);
	  
	  rg_partial_product0_1<=fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[5:0]);
	  rg_partial_product1_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[11:6])[101:0],6'd0};	  
	  rg_partial_product2_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[17:12])[95:0],12'd0};	  
	  rg_partial_product3_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[23:18])[89:0],18'd0};	  
	  rg_partial_product4_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[29:24])[83:0],24'd0};	  
	  rg_partial_product5_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[35:30])[77:0],30'd0};	  
	  rg_partial_product6_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[41:36])[71:0],36'd0};	  
	  rg_partial_product7_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[47:42])[65:0],42'd0};
	  rg_partial_product8_1<={fn_gen_pp_dp({(|expA1),sfdA1},{1'b0,(|expB1),sfdB1[51:48]})[59:0],48'd0};
	  
	endrule 
	
	Reg#(Bit#(108)) rg_smr1 <- mkReg(0);
	Reg#(Bit#(108)) rg_smr2 <- mkReg(0);
	Reg#(Bit#(108)) rg_smr3 <- mkReg(0);
	Reg#(Bit#(13)) rg_ers2<-mkReg(0);
	Reg#(Bit#(1)) rg_srs2<-mkReg(0);
	Reg#(Bit#(6)) rg_flags_s2 <-mkReg(0);
	rule rl_add_stage2;             
		rg_flags_s2 <= rg_flags_s1;
		rg_smr1<= fn_add_dp3(pack(rg_partial_product0_1),pack(rg_partial_product1_1),pack(rg_partial_product2_1));              
		rg_smr2<= fn_add_dp3(pack(rg_partial_product3_1),pack(rg_partial_product4_1),pack(rg_partial_product5_1));              
		rg_smr3<= fn_add_dp3(pack(rg_partial_product6_1),pack(rg_partial_product7_1),pack(rg_partial_product8_1));              
	 	  
	 /* $display("Stage2:");
	  $display("pp0: %h ", rg_partial_product0_1);
	  $display("pp1: %h ", rg_partial_product1_1);
	  $display("pp2: %h ", rg_partial_product2_1);
	  $display("pp3: %h ", rg_partial_product3_1);
	  $display("pp4: %h ", rg_partial_product4_1);
	  $display("pp5: %h ", rg_partial_product5_1);
	  $display("pp6: %h ", rg_partial_product6_1);
	  $display("pp7: %h ", rg_partial_product7_1);
	  $display("pp8: %h ", rg_partial_product8_1);*/
	  rg_ers2 <= rg_er;
	  rg_srs2 <= rg_sr;	
	  //$display("sign: %h ",rg_sr);  
	  //$display("exp: %h ",rg_er);
	  
	endrule
	Reg#(Bit#(13)) rg_ers3<-mkReg(0);
	Reg#(Bit#(1)) rg_srs3<-mkReg(0);
	Reg#(Bit#(108)) rg_smr <- mkReg(0);
	Reg#(Bit#(6)) rg_flags_s3 <-mkReg(0);
	rule rl_stage2o5;
		/*$display("Stage3:");
	 	$display(" sign: %h ", rg_srs2);
	 	$display(" exp: %h ", rg_ers2);*/
	 	//$display("s1: %h ", rg_smr1, " s2: %h ", rg_smr2, " s3: %h ", rg_smr3);
		rg_smr<= fn_add_dp3(pack(rg_smr1),pack(rg_smr2),pack(rg_smr3));   
		rg_ers3 <= rg_ers2;
	  rg_srs3 <= rg_srs2;	
	  rg_flags_s3 <= rg_flags_s2;
	endrule
	
	Reg#(Bit#(1)) rg_sign <- mkReg(0);
	Reg#(Bit#(13)) rg_ex <- mkReg(0);
	Reg#(Bit#(108)) rg_man <- mkReg(0);
	Reg#(Bit#(6)) rg_flags_s4 <-mkReg(0);
		//(* descending_urgency = "rl_eval_exponent_sign_stage1, rl_eval_partial_product_1_stage1, rl_add_stage2, rl_stage3" *)
	rule rl_stage3;
	 	/*$display("Stage4:");
	 	$display(" sign: %h ", rg_srs3);
	 	$display(" exp: %h ", rg_ers3);*/
	 	//$display(" smr: %h ", rg_smr);
	 	rg_flags_s4 <= rg_flags_s3;
		rg_sign <= rg_srs3;
		case (pack(rg_smr)[105:104])
			2'b00: begin
				rg_ex <= rg_ers3;
				rg_man <= rg_smr;
			end
			2'b01: begin
				rg_ex <= rg_ers3;
				rg_man <= {4'b0,pack(rg_smr)[103:0]};
			end
			2'b10: begin
				rg_ex <= rg_ers3+1;
				rg_man <= {4'b0,pack(rg_smr)[104:1]};
			end
			2'b11: begin
				rg_ex <= rg_ers3+1;
				rg_man <= {4'b0,pack(rg_smr)[104:1]};
			end
		endcase
		
	endrule
	
	Reg#(Bit#(1)) rg_sign_f <- mkReg(0);
	Reg#(Bit#(11)) rg_ex_f <- mkReg(0);
	Reg#(Bit#(104)) rg_man_f <- mkReg(0);
	Reg#(Bit#(6)) rg_flags_f <-mkReg(0);
	rule rl_stage5;
		rg_sign_f <= rg_sign;
		Bit#(1) ovf = ((rg_ex[12:11]==2'b01)||((rg_ex[12:11]==2'b00)&&(&rg_ex[10:0]==1'b1)))?1'b1:1'b0;		
		Bit#(1) sub_normal; //= ((|(diff & (rg_ex ^ negate(13'd53))))==1'b1)?1'b1:1'b0;
		UInt#(13) sub_const= 13'h1FCC;
		if(unpack(rg_ex) > sub_const)
			sub_normal=1'b1;
		else
			sub_normal=1'b0;
		
		Bit#(1) unf = (rg_ex[12]==1'b1)?1'b1:1'b0;// output exp=0 and fraction is non zero
		
		if(rg_flags_s4[2]==1'b1) begin // nan
			rg_ex_f <= 11'h7FF;
			rg_man_f <= 104'hFFFFFFFFFFFF;
			rg_flags_f <= {6'b000100};
		end
		else if(rg_flags_s4[0]==1'b1) begin // zero
			rg_ex_f <= 11'h0;
			rg_man_f <= 104'h0;
			rg_flags_f <= {6'b000001};
		end
		else if(rg_flags_s4[1]==1'b1) begin // inf
			rg_ex_f <= 11'h7FF;
			rg_man_f <= 104'h0;
			rg_flags_f <= {6'b000010};
		end
		else if(ovf==1'b1) begin // ovf
			rg_ex_f <= 11'h7FE;
			rg_man_f <= {pack(rg_man)[104:1]};
			rg_flags_f <= {6'b010000};
		end
		else if(sub_normal==1'b1) begin // sub normal output
			rg_ex_f <= 11'h0;
			rg_man_f <= {pack(rg_man)[104:1]};
			rg_flags_f <= {6'b100000};
		end
		else if(unf==1'b1) begin // unf
			rg_ex_f <= 11'h0;
			rg_man_f <= 104'h0000000000001;
			rg_flags_f <= {6'b001000};
		end
		else begin
			rg_ex_f <= rg_ex[10:0];
			rg_man_f <= rg_man[103:0];
			rg_flags_f <= rg_flags_s4;
		end
		
	
	endrule
	

	

	// send and receive methods
	method Action send(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52)) data_in);
	 d_copy1<=data_in;
	 d_copy2<=data_in;
	/* d_copy3<=data_in;
	 d_copy4<=data_in;
	 d_copy5<=data_in;
	 d_copy6<=data_in;
	 d_copy7<=data_in;
	 d_copy8<=data_in;
	 d_copy9<=data_in;
	 d_copy10<=data_in;
	 d_copy11<=data_in;*/
	endmethod


/*
method ReturnType#(8,23) receive();                                      
  return case (pack(rg_sp_ex3))
    3'b000:  ReturnType{valid: pack(rg_sp_valid[4]),value:tpl_1(rg_out),ex:tpl_2(rg_out)};
    3'b100:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: unpack(1'b0),exp: 8'hFF,sfd: truncate(24'h400000)},ex:unpack(5'b10000)};
    3'b010:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: tpl_1(rg_out).sign,exp: 8'hFF,sfd: 23'd0},ex:defaultValue};
    3'b001:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: tpl_1(rg_out).sign,exp: 8'd0,sfd: 23'd0},ex:defaultValue};
    3'b111:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: unpack(1'b0),exp: 8'hFF,sfd: truncate(24'h400000)},ex:unpack(5'b00000)};
   endcase;
endmethod
*/
method Tuple4#(Bit#(1),Bit#(11), Bit#(104), Bit#(6)) receive();                                      
	return tuple4(rg_sign_f, rg_ex_f, rg_man_f, rg_flags_f);
endmethod



endmodule: mkMulPipe
endpackage: TbMul
