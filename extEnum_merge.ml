let join_inner_by =
fun cmp_1  ->
  fun k1_2  ->
    fun k2_3  ->
      fun e1_4  ->
        fun e2_5  ->
          let _found_6 = Pervasives.ref false  in
          let rec next_7 () =
            let _prev_found_8 = false  in
            match ((Enum.peek e1_4), (Enum.peek e2_5)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_9,None ) -> (Enum.junk e1_4; next_7 ())
            | (None ,Some y_10) ->
                (Enum.junk e2_5; Pervasives.raise Enum.No_more_elements)
            | (Some x_11,Some y_12) ->
                let k1_13 = k1_2 x_11  in
                let k2_14 = k2_3 y_12  in
                (match cmp_1 k1_13 k2_14 with
                 | 0 -> ((); Enum.junk e2_5; (x_11, y_12))
                 | n_15 when n_15 < 0 -> (Enum.junk e1_4; next_7 ())
                 | _ -> (Enum.junk e2_5; next_7 ()))
             in
          Enum.from next_7

let join_inner_by_key cmp k = join_inner_by cmp k k

let join_left_by =
fun cmp_16  ->
  fun k1_17  ->
    fun k2_18  ->
      fun e1_19  ->
        fun e2_20  ->
          let _found_21 = Pervasives.ref false  in
          let rec next_22 () =
            let _prev_found_24 =
              let prev_23 = ! _found_21  in _found_21 := false; prev_23  in
            match ((Enum.peek e1_19), (Enum.peek e2_20)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_25,None ) ->
                (Enum.junk e1_19;
                 if Pervasives.not _prev_found_24
                 then (x_25, None)
                 else next_22 ())
            | (None ,Some y_26) ->
                (Enum.junk e2_20; Pervasives.raise Enum.No_more_elements)
            | (Some x_27,Some y_28) ->
                let k1_29 = k1_17 x_27  in
                let k2_30 = k2_18 y_28  in
                (match cmp_16 k1_29 k2_30 with
                 | 0 ->
                     (_found_21 := true; Enum.junk e2_20; (x_27, (Some y_28)))
                 | n_31 when n_31 < 0 ->
                     (Enum.junk e1_19;
                      if Pervasives.not _prev_found_24
                      then (x_27, None)
                      else next_22 ())
                 | _ -> (Enum.junk e2_20; next_22 ()))
             in
          Enum.from next_22

let join_left_by_key cmp k = join_left_by cmp k k

let join_right_by =
fun cmp_32  ->
  fun k1_33  ->
    fun k2_34  ->
      fun e1_35  ->
        fun e2_36  ->
          let _found_37 = Pervasives.ref false  in
          let rec next_38 () =
            let _prev_found_39 = false  in
            match ((Enum.peek e1_35), (Enum.peek e2_36)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_40,None ) -> (Enum.junk e1_35; next_38 ())
            | (None ,Some y_41) -> (Enum.junk e2_36; (None, y_41))
            | (Some x_42,Some y_43) ->
                let k1_44 = k1_33 x_42  in
                let k2_45 = k2_34 y_43  in
                (match cmp_32 k1_44 k2_45 with
                 | 0 -> ((); Enum.junk e2_36; ((Some x_42), y_43))
                 | n_46 when n_46 < 0 -> (Enum.junk e1_35; next_38 ())
                 | _ -> (Enum.junk e2_36; (None, y_43)))
             in
          Enum.from next_38

let join_right_by_key cmp k = join_right_by cmp k k

let join_full_by =
fun cmp_47  ->
  fun k1_48  ->
    fun k2_49  ->
      fun e1_50  ->
        fun e2_51  ->
          let _found_52 = Pervasives.ref false  in
          let rec next_53 () =
            let _prev_found_55 =
              let prev_54 = ! _found_52  in _found_52 := false; prev_54  in
            match ((Enum.peek e1_50), (Enum.peek e2_51)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_56,None ) ->
                (Enum.junk e1_50;
                 if Pervasives.not _prev_found_55
                 then `Left x_56
                 else next_53 ())
            | (None ,Some y_57) -> (Enum.junk e2_51; `Right y_57)
            | (Some x_58,Some y_59) ->
                let k1_60 = k1_48 x_58  in
                let k2_61 = k2_49 y_59  in
                (match cmp_47 k1_60 k2_61 with
                 | 0 ->
                     (_found_52 := true; Enum.junk e2_51; `Both (x_58, y_59))
                 | n_62 when n_62 < 0 ->
                     (Enum.junk e1_50;
                      if Pervasives.not _prev_found_55
                      then `Left x_58
                      else next_53 ())
                 | _ -> (Enum.junk e2_51; `Right y_59))
             in
          Enum.from next_53

let join_full_by_key cmp k = join_full_by cmp k k

let join_inner_uniq_by =
fun cmp_63  ->
  fun k1_64  ->
    fun k2_65  ->
      fun e1_66  ->
        fun e2_67  ->
          let _found_68 = Pervasives.ref false  in
          let rec next_69 () =
            let _prev_found_70 = false  in
            match ((Enum.peek e1_66), (Enum.peek e2_67)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_71,None ) -> (Enum.junk e1_66; next_69 ())
            | (None ,Some y_72) ->
                (Enum.junk e2_67; Pervasives.raise Enum.No_more_elements)
            | (Some x_73,Some y_74) ->
                let k1_75 = k1_64 x_73  in
                let k2_76 = k2_65 y_74  in
                (match cmp_63 k1_75 k2_76 with
                 | 0 -> (Enum.junk e1_66; Enum.junk e2_67; (x_73, y_74))
                 | n_77 when n_77 < 0 -> (Enum.junk e1_66; next_69 ())
                 | _ -> (Enum.junk e2_67; next_69 ()))
             in
          Enum.from next_69

let join_inner_uniq_by_key cmp k = join_inner_uniq_by cmp k k

let join_left_uniq_by =
fun cmp_78  ->
  fun k1_79  ->
    fun k2_80  ->
      fun e1_81  ->
        fun e2_82  ->
          let _found_83 = Pervasives.ref false  in
          let rec next_84 () =
            let _prev_found_85 = false  in
            match ((Enum.peek e1_81), (Enum.peek e2_82)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_86,None ) -> (Enum.junk e1_81; (x_86, None))
            | (None ,Some y_87) ->
                (Enum.junk e2_82; Pervasives.raise Enum.No_more_elements)
            | (Some x_88,Some y_89) ->
                let k1_90 = k1_79 x_88  in
                let k2_91 = k2_80 y_89  in
                (match cmp_78 k1_90 k2_91 with
                 | 0 ->
                     (Enum.junk e1_81; Enum.junk e2_82; (x_88, (Some y_89)))
                 | n_92 when n_92 < 0 -> (Enum.junk e1_81; (x_88, None))
                 | _ -> (Enum.junk e2_82; next_84 ()))
             in
          Enum.from next_84

let join_left_uniq_by_key cmp k = join_left_uniq_by cmp k k

let join_right_uniq_by =
fun cmp_93  ->
  fun k1_94  ->
    fun k2_95  ->
      fun e1_96  ->
        fun e2_97  ->
          let _found_98 = Pervasives.ref false  in
          let rec next_99 () =
            let _prev_found_100 = false  in
            match ((Enum.peek e1_96), (Enum.peek e2_97)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_101,None ) -> (Enum.junk e1_96; next_99 ())
            | (None ,Some y_102) -> (Enum.junk e2_97; (None, y_102))
            | (Some x_103,Some y_104) ->
                let k1_105 = k1_94 x_103  in
                let k2_106 = k2_95 y_104  in
                (match cmp_93 k1_105 k2_106 with
                 | 0 ->
                     (Enum.junk e1_96; Enum.junk e2_97; ((Some x_103), y_104))
                 | n_107 when n_107 < 0 -> (Enum.junk e1_96; next_99 ())
                 | _ -> (Enum.junk e2_97; (None, y_104)))
             in
          Enum.from next_99

let join_right_uniq_by_key cmp k = join_right_uniq_by cmp k k

let join_full_uniq_by =
fun cmp_108  ->
  fun k1_109  ->
    fun k2_110  ->
      fun e1_111  ->
        fun e2_112  ->
          let _found_113 = Pervasives.ref false  in
          let rec next_114 () =
            let _prev_found_115 = false  in
            match ((Enum.peek e1_111), (Enum.peek e2_112)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_116,None ) -> (Enum.junk e1_111; `Left x_116)
            | (None ,Some y_117) -> (Enum.junk e2_112; `Right y_117)
            | (Some x_118,Some y_119) ->
                let k1_120 = k1_109 x_118  in
                let k2_121 = k2_110 y_119  in
                (match cmp_108 k1_120 k2_121 with
                 | 0 ->
                     (Enum.junk e1_111;
                      Enum.junk e2_112;
                      `Both (x_118, y_119))
                 | n_122 when n_122 < 0 -> (Enum.junk e1_111; `Left x_118)
                 | _ -> (Enum.junk e2_112; `Right y_119))
             in
          Enum.from next_114

let join_full_uniq_by_key cmp k = join_full_uniq_by cmp k k

let join_assoc_inner =
fun cmp_123  ->
  fun e1_124  ->
    fun e2_125  ->
      let _found_126 = Pervasives.ref false  in
      let rec next_127 () =
        let _prev_found_128 = false  in
        match ((Enum.peek e1_124), (Enum.peek e2_125)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_129,None ) -> (Enum.junk e1_124; next_127 ())
        | (None ,Some y_130) ->
            (Enum.junk e2_125; Pervasives.raise Enum.No_more_elements)
        | (Some x_131,Some y_132) ->
            let k1_133 = Pervasives.fst x_131  in
            let k2_134 = Pervasives.fst y_132  in
            (match cmp_123 k1_133 k2_134 with
             | 0 ->
                 (();
                  Enum.junk e2_125;
                  (k1_133, (Pervasives.snd x_131), (Pervasives.snd y_132)))
             | n_135 when n_135 < 0 -> (Enum.junk e1_124; next_127 ())
             | _ -> (Enum.junk e2_125; next_127 ()))
         in
      Enum.from next_127

let join_assoc_left =
fun cmp_136  ->
  fun e1_137  ->
    fun e2_138  ->
      let _found_139 = Pervasives.ref false  in
      let rec next_140 () =
        let _prev_found_142 =
          let prev_141 = ! _found_139  in _found_139 := false; prev_141  in
        match ((Enum.peek e1_137), (Enum.peek e2_138)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_143,None ) ->
            (Enum.junk e1_137;
             if Pervasives.not _prev_found_142
             then ((Pervasives.fst x_143), (Pervasives.snd x_143), None)
             else next_140 ())
        | (None ,Some y_144) ->
            (Enum.junk e2_138; Pervasives.raise Enum.No_more_elements)
        | (Some x_145,Some y_146) ->
            let k1_147 = Pervasives.fst x_145  in
            let k2_148 = Pervasives.fst y_146  in
            (match cmp_136 k1_147 k2_148 with
             | 0 ->
                 (_found_139 := true;
                  Enum.junk e2_138;
                  (k1_147, (Pervasives.snd x_145),
                    (Some (Pervasives.snd y_146))))
             | n_149 when n_149 < 0 ->
                 (Enum.junk e1_137;
                  if Pervasives.not _prev_found_142
                  then (k1_147, (Pervasives.snd x_145), None)
                  else next_140 ())
             | _ -> (Enum.junk e2_138; next_140 ()))
         in
      Enum.from next_140

let join_assoc_right =
fun cmp_150  ->
  fun e1_151  ->
    fun e2_152  ->
      let _found_153 = Pervasives.ref false  in
      let rec next_154 () =
        let _prev_found_155 = false  in
        match ((Enum.peek e1_151), (Enum.peek e2_152)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_156,None ) -> (Enum.junk e1_151; next_154 ())
        | (None ,Some y_157) ->
            (Enum.junk e2_152;
             ((Pervasives.fst y_157), None, (Pervasives.snd y_157)))
        | (Some x_158,Some y_159) ->
            let k1_160 = Pervasives.fst x_158  in
            let k2_161 = Pervasives.fst y_159  in
            (match cmp_150 k1_160 k2_161 with
             | 0 ->
                 (();
                  Enum.junk e2_152;
                  (k1_160, (Some (Pervasives.snd x_158)),
                    (Pervasives.snd y_159)))
             | n_162 when n_162 < 0 -> (Enum.junk e1_151; next_154 ())
             | _ ->
                 (Enum.junk e2_152; (k2_161, None, (Pervasives.snd y_159))))
         in
      Enum.from next_154

let join_assoc_full =
fun cmp_163  ->
  fun e1_164  ->
    fun e2_165  ->
      let _found_166 = Pervasives.ref false  in
      let rec next_167 () =
        let _prev_found_169 =
          let prev_168 = ! _found_166  in _found_166 := false; prev_168  in
        match ((Enum.peek e1_164), (Enum.peek e2_165)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_170,None ) ->
            (Enum.junk e1_164;
             if Pervasives.not _prev_found_169
             then ((Pervasives.fst x_170), (`Left (Pervasives.snd x_170)))
             else next_167 ())
        | (None ,Some y_171) ->
            (Enum.junk e2_165;
             ((Pervasives.fst y_171), (`Right (Pervasives.snd y_171))))
        | (Some x_172,Some y_173) ->
            let k1_174 = Pervasives.fst x_172  in
            let k2_175 = Pervasives.fst y_173  in
            (match cmp_163 k1_174 k2_175 with
             | 0 ->
                 (_found_166 := true;
                  Enum.junk e2_165;
                  (k1_174,
                    (`Both ((Pervasives.snd x_172), (Pervasives.snd y_173)))))
             | n_176 when n_176 < 0 ->
                 (Enum.junk e1_164;
                  if Pervasives.not _prev_found_169
                  then (k1_174, (`Left (Pervasives.snd x_172)))
                  else next_167 ())
             | _ ->
                 (Enum.junk e2_165; (k2_175, (`Right (Pervasives.snd y_173)))))
         in
      Enum.from next_167

let join_assoc_inner_uniq =
fun cmp_177  ->
  fun e1_178  ->
    fun e2_179  ->
      let _found_180 = Pervasives.ref false  in
      let rec next_181 () =
        let _prev_found_182 = false  in
        match ((Enum.peek e1_178), (Enum.peek e2_179)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_183,None ) -> (Enum.junk e1_178; next_181 ())
        | (None ,Some y_184) ->
            (Enum.junk e2_179; Pervasives.raise Enum.No_more_elements)
        | (Some x_185,Some y_186) ->
            let k1_187 = Pervasives.fst x_185  in
            let k2_188 = Pervasives.fst y_186  in
            (match cmp_177 k1_187 k2_188 with
             | 0 ->
                 (Enum.junk e1_178;
                  Enum.junk e2_179;
                  (k1_187, (Pervasives.snd x_185), (Pervasives.snd y_186)))
             | n_189 when n_189 < 0 -> (Enum.junk e1_178; next_181 ())
             | _ -> (Enum.junk e2_179; next_181 ()))
         in
      Enum.from next_181

let join_assoc_left_uniq =
fun cmp_190  ->
  fun e1_191  ->
    fun e2_192  ->
      let _found_193 = Pervasives.ref false  in
      let rec next_194 () =
        let _prev_found_195 = false  in
        match ((Enum.peek e1_191), (Enum.peek e2_192)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_196,None ) ->
            (Enum.junk e1_191;
             ((Pervasives.fst x_196), (Pervasives.snd x_196), None))
        | (None ,Some y_197) ->
            (Enum.junk e2_192; Pervasives.raise Enum.No_more_elements)
        | (Some x_198,Some y_199) ->
            let k1_200 = Pervasives.fst x_198  in
            let k2_201 = Pervasives.fst y_199  in
            (match cmp_190 k1_200 k2_201 with
             | 0 ->
                 (Enum.junk e1_191;
                  Enum.junk e2_192;
                  (k1_200, (Pervasives.snd x_198),
                    (Some (Pervasives.snd y_199))))
             | n_202 when n_202 < 0 ->
                 (Enum.junk e1_191; (k1_200, (Pervasives.snd x_198), None))
             | _ -> (Enum.junk e2_192; next_194 ()))
         in
      Enum.from next_194

let join_assoc_right_uniq =
fun cmp_203  ->
  fun e1_204  ->
    fun e2_205  ->
      let _found_206 = Pervasives.ref false  in
      let rec next_207 () =
        let _prev_found_208 = false  in
        match ((Enum.peek e1_204), (Enum.peek e2_205)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_209,None ) -> (Enum.junk e1_204; next_207 ())
        | (None ,Some y_210) ->
            (Enum.junk e2_205;
             ((Pervasives.fst y_210), None, (Pervasives.snd y_210)))
        | (Some x_211,Some y_212) ->
            let k1_213 = Pervasives.fst x_211  in
            let k2_214 = Pervasives.fst y_212  in
            (match cmp_203 k1_213 k2_214 with
             | 0 ->
                 (Enum.junk e1_204;
                  Enum.junk e2_205;
                  (k1_213, (Some (Pervasives.snd x_211)),
                    (Pervasives.snd y_212)))
             | n_215 when n_215 < 0 -> (Enum.junk e1_204; next_207 ())
             | _ ->
                 (Enum.junk e2_205; (k2_214, None, (Pervasives.snd y_212))))
         in
      Enum.from next_207

let join_assoc_full_uniq =
fun cmp_216  ->
  fun e1_217  ->
    fun e2_218  ->
      let _found_219 = Pervasives.ref false  in
      let rec next_220 () =
        let _prev_found_221 = false  in
        match ((Enum.peek e1_217), (Enum.peek e2_218)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_222,None ) ->
            (Enum.junk e1_217;
             ((Pervasives.fst x_222), (`Left (Pervasives.snd x_222))))
        | (None ,Some y_223) ->
            (Enum.junk e2_218;
             ((Pervasives.fst y_223), (`Right (Pervasives.snd y_223))))
        | (Some x_224,Some y_225) ->
            let k1_226 = Pervasives.fst x_224  in
            let k2_227 = Pervasives.fst y_225  in
            (match cmp_216 k1_226 k2_227 with
             | 0 ->
                 (Enum.junk e1_217;
                  Enum.junk e2_218;
                  (k1_226,
                    (`Both ((Pervasives.snd x_224), (Pervasives.snd y_225)))))
             | n_228 when n_228 < 0 ->
                 (Enum.junk e1_217; (k1_226, (`Left (Pervasives.snd x_224))))
             | _ ->
                 (Enum.junk e2_218; (k2_227, (`Right (Pervasives.snd y_225)))))
         in
      Enum.from next_220

let merge =
fun cmp_229  ->
  fun e1_230  ->
    fun e2_231  ->
      let _found_232 = Pervasives.ref false  in
      let rec next_233 () =
        let _prev_found_234 = false  in
        match ((Enum.peek e1_230), (Enum.peek e2_231)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_235,None ) -> (Enum.junk e1_230; ((Some x_235), None))
        | (None ,Some y_236) -> (Enum.junk e2_231; (None, (Some y_236)))
        | (Some x_237,Some y_238) ->
            let k1_239 = x_237  in
            let k2_240 = y_238  in
            (match cmp_229 k1_239 k2_240 with
             | 0 ->
                 (Enum.junk e1_230;
                  Enum.junk e2_231;
                  ((Some x_237), (Some y_238)))
             | n_241 when n_241 < 0 ->
                 (Enum.junk e1_230; ((Some x_237), None))
             | _ -> (Enum.junk e2_231; (None, (Some y_238))))
         in
      Enum.from next_233

let merge_assoc =
fun cmp_242  ->
  fun e1_243  ->
    fun e2_244  ->
      let _found_245 = Pervasives.ref false  in
      let rec next_246 () =
        let _prev_found_247 = false  in
        match ((Enum.peek e1_243), (Enum.peek e2_244)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_248,None ) ->
            (Enum.junk e1_243;
             ((Pervasives.fst x_248), (Some (Pervasives.snd x_248)), None))
        | (None ,Some y_249) ->
            (Enum.junk e2_244;
             ((Pervasives.fst y_249), None, (Some (Pervasives.snd y_249))))
        | (Some x_250,Some y_251) ->
            let k1_252 = Pervasives.fst x_250  in
            let k2_253 = Pervasives.fst y_251  in
            (match cmp_242 k1_252 k2_253 with
             | 0 ->
                 (Enum.junk e1_243;
                  Enum.junk e2_244;
                  (k1_252, (Some (Pervasives.snd x_250)),
                    (Some (Pervasives.snd y_251))))
             | n_254 when n_254 < 0 ->
                 (Enum.junk e1_243;
                  (k1_252, (Some (Pervasives.snd x_250)), None))
             | _ ->
                 (Enum.junk e2_244;
                  (k2_253, None, (Some (Pervasives.snd y_251)))))
         in
      Enum.from next_246

