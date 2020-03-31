X_theta_3<-function(theta,t=tiempos, X_ini_3){
  SEIRmod <- function(t, x, theta) {
    with(as.list(c(theta, x)),
         {
           ds1 <- -bet1*s1*(is1+ia1) -delta1_2*s1 -delta1_3*s1 +
#           -delta1_4*s1 -delta1_5*s1 -delta1_6*s1 -delta1_7*s1 -delta1_8*s1 -delta1_9*s1 -delta1_10*s1 -delta1_11*s1 -delta1_12*s1 -delta1_13*s1 -delta1_14*s1 -delta1_15*s1 -delta1_16*s1 -delta1_17*s1 -delta1_18*s1 -delta1_19*s1 -delta1_20*s1 -delta1_21*s1 -delta1_22*s1 -delta1_23*s1 -delta1_24*s1 -delta1_25*s1 -delta1_26*s1 -delta1_27*s1 -delta1_28*s1 -delta1_29*s1 -delta1_30*s1 -delta1_31*s1 -delta1_32*s1
           +delta2_1*s2 +delta3_1*s3
           #+delta4_1*s4 +delta5_1*s5 +delta6_1*s6 +delta7_1*s7 +delta8_1*s8 +delta9_1*s9 +delta10_1*s10 +delta11_1*s11 +delta12_1*s12 +delta13_1*s13 +delta14_1*s14 +delta15_1*s15 +delta16_1*s16 +delta17_1*s17 +delta18_1*s18 +delta19_1*s19 +delta20_1*s20 +delta21_1*s21 +delta22_1*s22 +delta23_1*s23 +delta24_1*s24 +delta25_1*s25 +delta26_1*s26 +delta27_1*s27 +delta28_1*s28 +delta29_1*s29 +delta30_1*s30 +delta31_1*s31 +delta32_1*s32
           de1 <- bet1*s1*(is1+ia1)-sig*e1 -delta1_2*e1 -delta1_3*e1+
               #-delta1_4*e1 -delta1_5*e1 -delta1_6*e1 -delta1_7*e1 -delta1_8*e1 -delta1_9*e1 -delta1_10*e1 -delta1_11*e1 -delta1_12*e1 -delta1_13*e1 -delta1_14*e1 -delta1_15*e1 -delta1_16*e1 -delta1_17*e1 -delta1_18*e1 -delta1_19*e1 -delta1_20*e1 -delta1_21*e1 -delta1_22*e1 -delta1_23*e1 -delta1_24*e1 -delta1_25*e1 -delta1_26*e1 -delta1_27*e1 -delta1_28*e1 -delta1_29*e1 -delta1_30*e1 -delta1_31*e1 -delta1_32*e1
               delta2_1*e2 +delta3_1*e3
               #+delta4_1*e4 +delta5_1*e5 +delta6_1*e6 +delta7_1*e7 +delta8_1*e8 +delta9_1*e9 +delta10_1*e10 +delta11_1*e11 +delta12_1*e12 +delta13_1*e13 +delta14_1*e14 +delta15_1*e15 +delta16_1*e16 +delta17_1*e17 +delta18_1*e18 +delta19_1*e19 +delta20_1*e20 +delta21_1*e21 +delta22_1*e22 +delta23_1*e23 +delta24_1*e24 +delta25_1*e25 +delta26_1*e26 +delta27_1*e27 +delta28_1*e28 +delta29_1*e29 +delta30_1*e30 +delta31_1*e31 +delta32_1*e32
           dis1 <- sig*(1-alf)*e1-gams*is1 -delta1_2*is1 -delta1_3*is1 +
               #-delta1_4*is1 -delta1_5*is1 -delta1_6*is1 -delta1_7*is1 -delta1_8*is1 -delta1_9*is1 -delta1_10*is1 -delta1_11*is1 -delta1_12*is1 -delta1_13*is1 -delta1_14*is1 -delta1_15*is1 -delta1_16*is1 -delta1_17*is1 -delta1_18*is1 -delta1_19*is1 -delta1_20*is1 -delta1_21*is1 -delta1_22*is1 -delta1_23*is1 -delta1_24*is1 -delta1_25*is1 -delta1_26*is1 -delta1_27*is1 -delta1_28*is1 -delta1_29*is1 -delta1_30*is1 -delta1_31*is1 -delta1_32*is1
           +delta2_1*is2 +delta3_1*is3
           #+delta4_1*is4 +delta5_1*is5 +delta6_1*is6 +delta7_1*is7 +delta8_1*is8 +delta9_1*is9 +delta10_1*is10 +delta11_1*is11 +delta12_1*is12 +delta13_1*is13 +delta14_1*is14 +delta15_1*is15 +delta16_1*is16 +delta17_1*is17 +delta18_1*is18 +delta19_1*is19 +delta20_1*is20 +delta21_1*is21 +delta22_1*is22 +delta23_1*is23 +delta24_1*is24 +delta25_1*is25 +delta26_1*is26 +delta27_1*is27 +delta28_1*is28 +delta29_1*is29 +delta30_1*is30 +delta31_1*is31 +delta32_1*is32
           dia1 <- sig*(alf)*e1-gama*ia1 -delta1_2*ia1 -delta1_3*ia1+
               #-delta1_4*ia1 -delta1_5*ia1 -delta1_6*ia1 -delta1_7*ia1 -delta1_8*ia1 -delta1_9*ia1 -delta1_10*ia1 -delta1_11*ia1 -delta1_12*ia1 -delta1_13*ia1 -delta1_14*ia1 -delta1_15*ia1 -delta1_16*ia1 -delta1_17*ia1 -delta1_18*ia1 -delta1_19*ia1 -delta1_20*ia1 -delta1_21*ia1 -delta1_22*ia1 -delta1_23*ia1 -delta1_24*ia1 -delta1_25*ia1 -delta1_26*ia1 -delta1_27*ia1 -delta1_28*ia1 -delta1_29*ia1 -delta1_30*ia1 -delta1_31*ia1 -delta1_32*ia1
               delta2_1*ia2 +delta3_1*ia3
           # +delta4_1*ia4 +delta5_1*ia5 +delta6_1*ia6 +delta7_1*ia7 +delta8_1*ia8 +delta9_1*ia9 +delta10_1*ia10 +delta11_1*ia11 +delta12_1*ia12 +delta13_1*ia13 +delta14_1*ia14 +delta15_1*ia15 +delta16_1*ia16 +delta17_1*ia17 +delta18_1*ia18 +delta19_1*ia19 +delta20_1*ia20 +delta21_1*ia21 +delta22_1*ia22 +delta23_1*ia23 +delta24_1*ia24 +delta25_1*ia25 +delta26_1*ia26 +delta27_1*ia27 +delta28_1*ia28 +delta29_1*ia29 +delta30_1*ia30 +delta31_1*ia31 +delta32_1*ia32
           dr1 <- gams*is1+gama*ia1 -delta1_2*r1 -delta1_3*r1 +
               #-delta1_4*r1 -delta1_5*r1 -delta1_6*r1 -delta1_7*r1 -delta1_8*r1 -delta1_9*r1 -delta1_10*r1 -delta1_11*r1 -delta1_12*r1 -delta1_13*r1 -delta1_14*r1 -delta1_15*r1 -delta1_16*r1 -delta1_17*r1 -delta1_18*r1 -delta1_19*r1 -delta1_20*r1 -delta1_21*r1 -delta1_22*r1 -delta1_23*r1 -delta1_24*r1 -delta1_25*r1 -delta1_26*r1 -delta1_27*r1 -delta1_28*r1 -delta1_29*r1 -delta1_30*r1 -delta1_31*r1 -delta1_32*r1
               delta2_1*r2 +delta3_1*r3
           #+delta4_1*r4 +delta5_1*r5 +delta6_1*r6 +delta7_1*r7 +delta8_1*r8 +delta9_1*r9 +delta10_1*r10 +delta11_1*r11 +delta12_1*r12 +delta13_1*r13 +delta14_1*r14 +delta15_1*r15 +delta16_1*r16 +delta17_1*r17 +delta18_1*r18 +delta19_1*r19 +delta20_1*r20 +delta21_1*r21 +delta22_1*r22 +delta23_1*r23 +delta24_1*r24 +delta25_1*r25 +delta26_1*r26 +delta27_1*r27 +delta28_1*r28 +delta29_1*r29 +delta30_1*r30 +delta31_1*r31 +delta32_1*r32
           dy1 <-sig*(1-alf)*e1
           ds2 <- -bet2*s2*(is2+ia2) -delta2_1*s2 -delta2_3*s2 +
               #-delta2_4*s2 -delta2_5*s2 -delta2_6*s2 -delta2_7*s2 -delta2_8*s2 -delta2_9*s2 -delta2_10*s2 -delta2_11*s2 -delta2_12*s2 -delta2_13*s2 -delta2_14*s2 -delta2_15*s2 -delta2_16*s2 -delta2_17*s2 -delta2_18*s2 -delta2_19*s2 -delta2_20*s2 -delta2_21*s2 -delta2_22*s2 -delta2_23*s2 -delta2_24*s2 -delta2_25*s2 -delta2_26*s2 -delta2_27*s2 -delta2_28*s2 -delta2_29*s2 -delta2_30*s2 -delta2_31*s2 -delta2_32*s2
               delta1_2*s1 +delta3_2*s3
           #+delta4_2*s4 +delta5_2*s5 +delta6_2*s6 +delta7_2*s7 +delta8_2*s8 +delta9_2*s9 +delta10_2*s10 +delta11_2*s11 +delta12_2*s12 +delta13_2*s13 +delta14_2*s14 +delta15_2*s15 +delta16_2*s16 +delta17_2*s17 +delta18_2*s18 +delta19_2*s19 +delta20_2*s20 +delta21_2*s21 +delta22_2*s22 +delta23_2*s23 +delta24_2*s24 +delta25_2*s25 +delta26_2*s26 +delta27_2*s27 +delta28_2*s28 +delta29_2*s29 +delta30_2*s30 +delta31_2*s31 +delta32_2*s32
           de2 <- bet2*s2*(is2+ia2)-sig*e2 -delta2_1*e2 -delta2_3*e2+
               # -delta2_4*e2 -delta2_5*e2 -delta2_6*e2 -delta2_7*e2 -delta2_8*e2 -delta2_9*e2 -delta2_10*e2 -delta2_11*e2 -delta2_12*e2 -delta2_13*e2 -delta2_14*e2 -delta2_15*e2 -delta2_16*e2 -delta2_17*e2 -delta2_18*e2 -delta2_19*e2 -delta2_20*e2 -delta2_21*e2 -delta2_22*e2 -delta2_23*e2 -delta2_24*e2 -delta2_25*e2 -delta2_26*e2 -delta2_27*e2 -delta2_28*e2 -delta2_29*e2 -delta2_30*e2 -delta2_31*e2
               #-delta2_32*e2 +
               delta1_2*e1 +delta3_2*e3
           # +delta4_2*e4 +delta5_2*e5 +delta6_2*e6 +delta7_2*e7 +delta8_2*e8 +delta9_2*e9 +delta10_2*e10 +delta11_2*e11 +delta12_2*e12 +delta13_2*e13 +delta14_2*e14 +delta15_2*e15 +delta16_2*e16 +delta17_2*e17 +delta18_2*e18 +delta19_2*e19 +delta20_2*e20 +delta21_2*e21 +delta22_2*e22 +delta23_2*e23 +delta24_2*e24 +delta25_2*e25 +delta26_2*e26 +delta27_2*e27 +delta28_2*e28 +delta29_2*e29 +delta30_2*e30 +delta31_2*e31 +delta32_2*e32
           dis2 <- sig*(1-alf)*e2-gams*is2 -delta2_1*is2 -delta2_3*is2 +
               #-delta2_4*is2 -delta2_5*is2 -delta2_6*is2 -delta2_7*is2 -delta2_8*is2 -delta2_9*is2 -delta2_10*is2 -delta2_11*is2 -delta2_12*is2 -delta2_13*is2 -delta2_14*is2 -delta2_15*is2 -delta2_16*is2 -delta2_17*is2 -delta2_18*is2 -delta2_19*is2 -delta2_20*is2 -delta2_21*is2 -delta2_22*is2 -delta2_23*is2 -delta2_24*is2 -delta2_25*is2 -delta2_26*is2 -delta2_27*is2 -delta2_28*is2 -delta2_29*is2 -delta2_30*is2 -delta2_31*is2 -delta2_32*is2
               delta1_2*is1 +delta3_2*is3 #+delta4_2*is4 +delta5_2*is5 +delta6_2*is6 +delta7_2*is7 +delta8_2*is8 +delta9_2*is9 +delta10_2*is10 +delta11_2*is11 +delta12_2*is12 +delta13_2*is13 +delta14_2*is14 +delta15_2*is15 +delta16_2*is16 +delta17_2*is17 +delta18_2*is18 +delta19_2*is19 +delta20_2*is20 +delta21_2*is21 +delta22_2*is22 +delta23_2*is23 +delta24_2*is24 +delta25_2*is25 +delta26_2*is26 +delta27_2*is27 +delta28_2*is28 +delta29_2*is29 +delta30_2*is30 +delta31_2*is31 +delta32_2*is32
           dia2 <- sig*(alf)*e2-gama*ia2 -delta2_1*ia2 -delta2_3*ia2 +
               #-delta2_4*ia2 -delta2_5*ia2 -delta2_6*ia2 -delta2_7*ia2 -delta2_8*ia2 -delta2_9*ia2 -delta2_10*ia2 -delta2_11*ia2 -delta2_12*ia2 -delta2_13*ia2 -delta2_14*ia2 -delta2_15*ia2 -delta2_16*ia2 -delta2_17*ia2 -delta2_18*ia2 -delta2_19*ia2 -delta2_20*ia2 -delta2_21*ia2 -delta2_22*ia2 -delta2_23*ia2 -delta2_24*ia2 -delta2_25*ia2 -delta2_26*ia2 -delta2_27*ia2 -delta2_28*ia2 -delta2_29*ia2 -delta2_30*ia2 -delta2_31*ia2 -delta2_32*ia2 +
               delta1_2*ia1 +delta3_2*ia3# +delta4_2*ia4 +delta5_2*ia5 +delta6_2*ia6 +delta7_2*ia7 +delta8_2*ia8 +delta9_2*ia9 +delta10_2*ia10 +delta11_2*ia11 +delta12_2*ia12 +delta13_2*ia13 +delta14_2*ia14 +delta15_2*ia15 +delta16_2*ia16 +delta17_2*ia17 +delta18_2*ia18 +delta19_2*ia19 +delta20_2*ia20 +delta21_2*ia21 +delta22_2*ia22 +delta23_2*ia23 +delta24_2*ia24 +delta25_2*ia25 +delta26_2*ia26 +delta27_2*ia27 +delta28_2*ia28 +delta29_2*ia29 +delta30_2*ia30 +delta31_2*ia31 +delta32_2*ia32
           dr2 <- gams*is2+gama*ia2 -delta2_1*r2 -delta2_3*r2+#
              # -delta2_4*r2 -delta2_5*r2 -delta2_6*r2 -delta2_7*r2 -delta2_8*r2 -delta2_9*r2 -delta2_10*r2 -delta2_11*r2 -delta2_12*r2 -delta2_13*r2 -delta2_14*r2 -delta2_15*r2 -delta2_16*r2 -delta2_17*r2 -delta2_18*r2 -delta2_19*r2 -delta2_20*r2 -delta2_21*r2 -delta2_22*r2 -delta2_23*r2 -delta2_24*r2 -delta2_25*r2 -delta2_26*r2 -delta2_27*r2 -delta2_28*r2 -delta2_29*r2 -delta2_30*r2 -delta2_31*r2 -delta2_32*r2 +
               delta1_2*r1 +delta3_2*r3 #+delta4_2*r4 +delta5_2*r5 +delta6_2*r6 +delta7_2*r7 +delta8_2*r8 +delta9_2*r9 +delta10_2*r10 +delta11_2*r11 +delta12_2*r12 +delta13_2*r13 +delta14_2*r14 +delta15_2*r15 +delta16_2*r16 +delta17_2*r17 +delta18_2*r18 +delta19_2*r19 +delta20_2*r20 +delta21_2*r21 +delta22_2*r22 +delta23_2*r23 +delta24_2*r24 +delta25_2*r25 +delta26_2*r26 +delta27_2*r27 +delta28_2*r28 +delta29_2*r29 +delta30_2*r30 +delta31_2*r31 +delta32_2*r32
           dy2 <-sig*(1-alf)*e2



           ds3 <- -bet3*s3*(is3+ia3) -delta3_1*s3 -delta3_2*s3+
               # -delta3_4*s3 -delta3_5*s3 -delta3_6*s3 -delta3_7*s3 -delta3_8*s3 -delta3_9*s3 -delta3_10*s3 -delta3_11*s3 -delta3_12*s3 -delta3_13*s3 -delta3_14*s3 -delta3_15*s3 -delta3_16*s3 -delta3_17*s3 -delta3_18*s3 -delta3_19*s3 -delta3_20*s3 -delta3_21*s3 -delta3_22*s3 -delta3_23*s3 -delta3_24*s3 -delta3_25*s3 -delta3_26*s3 -delta3_27*s3 -delta3_28*s3 -delta3_29*s3 -delta3_30*s3 -delta3_31*s3 -delta3_32*s3
               delta1_3*s1 +delta2_3*s2# +delta4_3*s4 +delta5_3*s5 +delta6_3*s6 +delta7_3*s7 +delta8_3*s8 +delta9_3*s9 +delta10_3*s10 +delta11_3*s11 +delta12_3*s12 +delta13_3*s13 +delta14_3*s14 +delta15_3*s15 +delta16_3*s16 +delta17_3*s17 +delta18_3*s18 +delta19_3*s19 +delta20_3*s20 +delta21_3*s21 +delta22_3*s22 +delta23_3*s23 +delta24_3*s24 +delta25_3*s25 +delta26_3*s26 +delta27_3*s27 +delta28_3*s28 +delta29_3*s29 +delta30_3*s30 +delta31_3*s31 +delta32_3*s32

            de3 <- bet3*s3*(is3+ia3)-sig*e3 -delta3_1*e3 -delta3_2*e3+
               #-delta3_4*e3 -delta3_5*e3 -delta3_6*e3 -delta3_7*e3 -delta3_8*e3 -delta3_9*e3 -delta3_10*e3 -delta3_11*e3 -delta3_12*e3 -delta3_13*e3 -delta3_14*e3 -delta3_15*e3 -delta3_16*e3 -delta3_17*e3 -delta3_18*e3 -delta3_19*e3 -delta3_20*e3 -delta3_21*e3 -delta3_22*e3 -delta3_23*e3 -delta3_24*e3 -delta3_25*e3 -delta3_26*e3 -delta3_27*e3 -delta3_28*e3 -delta3_29*e3 -delta3_30*e3 -delta3_31*e3 -delta3_32*e3 +
                delta1_3*e1 +delta2_3*e2 #+delta4_3*e4 +delta5_3*e5 +delta6_3*e6 +delta7_3*e7 +delta8_3*e8 +delta9_3*e9 +delta10_3*e10 +delta11_3*e11 +delta12_3*e12 +delta13_3*e13 +delta14_3*e14 +delta15_3*e15 +delta16_3*e16 +delta17_3*e17 +delta18_3*e18 +delta19_3*e19 +delta20_3*e20 +delta21_3*e21 +delta22_3*e22 +delta23_3*e23 +delta24_3*e24 +delta25_3*e25 +delta26_3*e26 +delta27_3*e27 +delta28_3*e28 +delta29_3*e29 +delta30_3*e30 +delta31_3*e31 +delta32_3*e32

            dis3 <- sig*(1-alf)*e3-gams*is3 -delta3_1*is3 -delta3_2*is3+
               #delta3_4*is3 -delta3_5*is3 -delta3_6*is3 -delta3_7*is3 -delta3_8*is3 -delta3_9*is3 -delta3_10*is3 -delta3_11*is3 -delta3_12*is3 -delta3_13*is3 -delta3_14*is3 -delta3_15*is3 -delta3_16*is3 -delta3_17*is3 -delta3_18*is3 -delta3_19*is3 -delta3_20*is3 -delta3_21*is3 -delta3_22*is3 -delta3_23*is3 -delta3_24*is3 -delta3_25*is3 -delta3_26*is3 -delta3_27*is3 -delta3_28*is3 -delta3_29*is3 -delta3_30*is3 -delta3_31*is3 -delta3_32*is3
                delta1_3*is1 +delta2_3*is2 #+delta4_3*is4 +delta5_3*is5 +delta6_3*is6 +delta7_3*is7 +delta8_3*is8 +delta9_3*is9 +delta10_3*is10 +delta11_3*is11 +delta12_3*is12 +delta13_3*is13 +delta14_3*is14 +delta15_3*is15 +delta16_3*is16 +delta17_3*is17 +delta18_3*is18 +delta19_3*is19 +delta20_3*is20 +delta21_3*is21 +delta22_3*is22 +delta23_3*is23 +delta24_3*is24 +delta25_3*is25 +delta26_3*is26 +delta27_3*is27 +delta28_3*is28 +delta29_3*is29 +delta30_3*is30 +delta31_3*is31 +delta32_3*is32
           dia3 <- sig*(alf)*e3-gama*ia3 -delta3_1*ia3 -delta3_2*ia3 +
               #-delta3_4*ia3 -delta3_5*ia3 -delta3_6*ia3 -delta3_7*ia3 -delta3_8*ia3 -delta3_9*ia3 -delta3_10*ia3 -delta3_11*ia3 -delta3_12*ia3 -delta3_13*ia3 -delta3_14*ia3 -delta3_15*ia3 -delta3_16*ia3 -delta3_17*ia3 -delta3_18*ia3 -delta3_19*ia3 -delta3_20*ia3 -delta3_21*ia3 -delta3_22*ia3 -delta3_23*ia3 -delta3_24*ia3 -delta3_25*ia3 -delta3_26*ia3 -delta3_27*ia3 -delta3_28*ia3 -delta3_29*ia3 -delta3_30*ia3 -delta3_31*ia3 -delta3_32*ia3 +
               delta1_3*ia1 +delta2_3*ia2# +delta4_3*ia4 +delta5_3*ia5 +delta6_3*ia6 +delta7_3*ia7 +delta8_3*ia8 +delta9_3*ia9 +delta10_3*ia10 +delta11_3*ia11 +delta12_3*ia12 +delta13_3*ia13 +delta14_3*ia14 +delta15_3*ia15 +delta16_3*ia16 +delta17_3*ia17 +delta18_3*ia18 +delta19_3*ia19 +delta20_3*ia20 +delta21_3*ia21 +delta22_3*ia22 +delta23_3*ia23 +delta24_3*ia24 +delta25_3*ia25 +delta26_3*ia26 +delta27_3*ia27 +delta28_3*ia28 +delta29_3*ia29 +delta30_3*ia30 +delta31_3*ia31 +delta32_3*ia32
           dr3 <- gams*is3+gama*ia3 -delta3_1*r3 -delta3_2*r3 +
               #-delta3_4*r3 -delta3_5*r3 -delta3_6*r3 -delta3_7*r3 -delta3_8*r3 -delta3_9*r3 -delta3_10*r3 -delta3_11*r3 -delta3_12*r3 -delta3_13*r3 -delta3_14*r3 -delta3_15*r3 -delta3_16*r3 -delta3_17*r3 -delta3_18*r3 -delta3_19*r3 -delta3_20*r3 -delta3_21*r3 -delta3_22*r3 -delta3_23*r3 -delta3_24*r3 -delta3_25*r3 -delta3_26*r3 -delta3_27*r3 -delta3_28*r3 -delta3_29*r3 -delta3_30*r3 -delta3_31*r3 -delta3_32*r3 +
               delta1_3*r1 +delta2_3*r2# +delta4_3*r4 +delta5_3*r5 +delta6_3*r6 +delta7_3*r7 +delta8_3*r8 +delta9_3*r9 +delta10_3*r10 +delta11_3*r11 +delta12_3*r12 +delta13_3*r13 +delta14_3*r14 +delta15_3*r15 +delta16_3*r16 +delta17_3*r17 +delta18_3*r18 +delta19_3*r19 +delta20_3*r20 +delta21_3*r21 +delta22_3*r22 +delta23_3*r23 +delta24_3*r24 +delta25_3*r25 +delta26_3*r26 +delta27_3*r27 +delta28_3*r28 +delta29_3*r29 +delta30_3*r30 +delta31_3*r31 +delta32_3*r32
           dy3 <-sig*(1-alf)*e3


           res <- c(ds1, ds2, ds3,
                    de1, de2, de3,
                    dis1, dis2, dis3,
                    dia1, dia2, dia3,
                    dr1, dr2, dr3,
                    dy1, dy2, dy3
           )
           list(res) }
    ) }
  ## Solver
  out <- lsoda(X_ini, t, SEIRmod, theta)
  out[which(out[,3]<0),3]<-0
  return(out)
}

