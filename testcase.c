
// gcc -o testcase testcase.c

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
int setk = 0;


uint64_t decimalToIEEE754(double value) {
    
    int sign,exponent = 0 ; double mantissa;
    uint64_t sign_bit,shifted_exponent, mantissa_bits,result;


    sign = (value < 0) ? 1 : 0;
    value = (sign) ? -value : value;
    printf("\nsign:%d\n",sign);

    // Special case: positive zero
    if (value == 0.0) {
        return 0x0000000000000000; 
    }
   
    while (value >= 2.0) {
        value = value / 2.0;
        exponent++;
    }
    while (value < 1.0) {
        value = value * 2.0;
        exponent--;
    }

   
    exponent += 1023;    
    mantissa = value - 1.0;

    sign_bit = (uint64_t)sign << 63;    
    shifted_exponent = ((exponent >= 0) && (exponent < 64)) ? ((uint64_t)exponent << 52) : 0;
   
    mantissa_bits = (mantissa < 1.0) ? (uint64_t)(mantissa * (1ULL << 52)) & 0xFFFFFFFFFFFFF : 0;

     
     printf("\nrule rl_send%d(rg_cycle==%d);\n", setk, setk);
     printf("\n\tFloatingPoint#(11, 52) f,g; \n");
     if(sign_bit ==1)
     printf("\n\tf.sign = %s;\n","True");
     else
     printf("\n\tf.sign = %s;\n","False");
     printf("\n\tf.exp = 3h'%lx;\n",shifted_exponent);
     printf("\n\tf.sfd = 9h'%lx;\n",mantissa_bits);
     printf("\n\n");
     setk++;


    result = sign_bit | shifted_exponent | mantissa_bits;

    return result;
}
double generate_random_double() {

    uint64_t sign_bit, exponent, exponent_bits, fraction, ieee754_bits;
    double result;

     sign_bit = rand() % 2; 
     exponent = (rand() % 2047); 
     exponent_bits = (exponent >= 0 && exponent < 64) ? ((uint64_t)exponent << 52) : 0;

     fraction = rand();     
     ieee754_bits = (sign_bit << 63) | (exponent << 52) | (fraction >> 12); 
    
    memcpy(&result, &ieee754_bits, sizeof(result));

    return result;
}

void main()
{


/// UnComment this part to generate random test vectors.

    int i = 0;
    double decimalValue = generate_random_double();
    uint64_t ieee754Value = decimalToIEEE754(decimalValue);
    


    printf("Decimal value: %lf\n", decimalValue);
    printf("IEEE 754 representation: 0x%lX\n", ieee754Value);

   for (int i = 0; i < 15; ++i) {
        double test_case = generate_random_double();
        srand((unsigned int)time(NULL));
        printf("Test Case %d:\n", i + 1);
        printf("Value: %f\n", test_case);
        printf("\n");

        uint64_t ieee754Value = decimalToIEEE754(decimalValue);

        printf("Decimal value: %lf\n", decimalValue);
        printf("IEEE 754 representation: 0x%lX\n", ieee754Value);
        
    }

/// UnComment this part to use the hard coded value
char fsign[16][20] = {"False",
    "False",
    "True",
    "True",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False"
    };



    char gsign[16][20] = {"False",
    "True",
    "True",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "False",
    "True",
    "True",
    "False",
    "True",
    "False"};


    char fexp[16][20] = {"11b'01111111111",
    "11b'10000000000",
    "11b'10000000010",
    "11b'10000000001",
    "11b'00000000000",
    "11b'11111111110",
    "11b'00000000000",
    "11b'01111111111",
    "11b'11010010111",
    "11b'00000000000",
    "11b'11111111111",
    "11b'10000000000",
    "11b'11111111111",
    "11b'10000000001",
    "11b'01111111011"};


    char gexp[16][20] = {"11b'10000001011",
    "11b'10000000111",
    "11b'10000000101",
    "11b'10000001000",
    "11b'10000000101",
    "11b'11111111111",
    "11b'00000000000",
    "11b'01111100100",
    "11b'11010010111",
    "11b'00000000000",
    "11b'10000000000",
    "11b'11111111111",
    "11b'10000000001",
    "11b'11111111111",
    "11b'01111111100"};

    char fsfd[16][60] = {"52b'1010110010101100000010000011000100100110111010010111",
    "52b'1110001111010111000010100011110101110000101000111101",
    "52b'0011101111010111000010100011110101110000101000111101",
    "52b'0010101011100001010001111010111000010100011110101110",
    "52b'0000000000000000000000000000000000000000000000000000",
    "52b'0001110011001111001110000101111010111100100010011111",
    "52b'0111001100001101011001111000000110011110100011010010",
    "52b'0011110000001100101001000010100000111101111000011011",
    "52b'0100111001110001100011010111110101110110001001011010",
    "52b'0000000000000000000000000000000000000000000000000010",
    "52b'1111111111111111111111111111111111111111111111111111",
    "52b'1001000111101011100001010001111010111000010100011111",
    "52b'0000000000000000000000000000000000000000000000000000",
    "52b'0100000000000000000000000000000000000000000000000000",
    "52b'1001100110011001100110011001100110011001100110011010"};

    char gsfd[16][60] = {"52b'0010010001100101011100001010001111010111000010100011",
    "52b'0100101110101011100001010001111010111000010100011111",
    "52b'0110011100001010001111010111000010100011110101110001",
    "52b'0001101111110001111010111000010100011110101110000101",
    "52b'0110010110011001100110011001100110011001100110011010",
    "52b'0000000000000000000000000000000000000000000000000000",
    "52b'1110011000011010110011110000001100111101000110100100",
    "52b'0101011110011000111011100010001100001000110000111010",
    "52b'0100111001110001100011010111110101110110001001011010",
    "52b'0000000000000000000000000000000000000000000000000100",
    "52b'1001000111101011100001010001111010111000010100011111",
    "52b'1111111111111111111111111111111111111111111111111111",
    "52b'0100000000000000000000000000000000000000000000000000",
    "52b'0000000000000000000000000000000000000000000000000000",
    "52b'1001100110011001100110011001100110011001100110011010"};



    for (i = 0; i < 15; i++)
    {

     printf("\nrule rl_send%d(rg_cycle==%d);\n", i, i);
     printf("\n\tFloatingPoint#(11, 52) f,g; \n");
     printf("\n\tf.sign = %s;\n",fsign[i]);
     printf("\n\tf.exp = %s;\n",fexp[i]);
     printf("\n\tf.sfd = %s;\n",fsfd[i]);
     printf("\n\n");

     printf("\n\tg.sign = %s;\n",gsign[i]);
     printf("\n\tg.exp =  %s;\n",gexp[i]);
     printf("\n\tg.sfd = %s;\n",gsfd[i]);
     printf("\n");

     printf("\n\tpipe.send(tuple2(f,g));\n");
     printf("\n\t$display(\"cycle:\",rg_cycle,\" \",fshow(f),fshow(g));");
     printf("\nendrule\n");

    }

}