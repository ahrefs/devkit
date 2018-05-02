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
                 | 0 -> (Enum.junk e1_4; Enum.junk e2_5; (x_11, y_12))
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
            let _prev_found_23 = false  in
            match ((Enum.peek e1_19), (Enum.peek e2_20)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_24,None ) -> (Enum.junk e1_19; (x_24, None))
            | (None ,Some y_25) ->
                (Enum.junk e2_20; Pervasives.raise Enum.No_more_elements)
            | (Some x_26,Some y_27) ->
                let k1_28 = k1_17 x_26  in
                let k2_29 = k2_18 y_27  in
                (match cmp_16 k1_28 k2_29 with
                 | 0 ->
                     (Enum.junk e1_19; Enum.junk e2_20; (x_26, (Some y_27)))
                 | n_30 when n_30 < 0 -> (Enum.junk e1_19; (x_26, None))
                 | _ -> (Enum.junk e2_20; next_22 ()))
             in
          Enum.from next_22

let join_left_by_key cmp k = join_left_by cmp k k

let join_right_by =
fun cmp_31  ->
  fun k1_32  ->
    fun k2_33  ->
      fun e1_34  ->
        fun e2_35  ->
          let _found_36 = Pervasives.ref false  in
          let rec next_37 () =
            let _prev_found_38 = false  in
            match ((Enum.peek e1_34), (Enum.peek e2_35)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_39,None ) -> (Enum.junk e1_34; next_37 ())
            | (None ,Some y_40) -> (Enum.junk e2_35; (None, y_40))
            | (Some x_41,Some y_42) ->
                let k1_43 = k1_32 x_41  in
                let k2_44 = k2_33 y_42  in
                (match cmp_31 k1_43 k2_44 with
                 | 0 ->
                     (Enum.junk e1_34; Enum.junk e2_35; ((Some x_41), y_42))
                 | n_45 when n_45 < 0 -> (Enum.junk e1_34; next_37 ())
                 | _ -> (Enum.junk e2_35; (None, y_42)))
             in
          Enum.from next_37

let join_right_by_key cmp k = join_right_by cmp k k

let join_full_by =
fun cmp_46  ->
  fun k1_47  ->
    fun k2_48  ->
      fun e1_49  ->
        fun e2_50  ->
          let _found_51 = Pervasives.ref false  in
          let rec next_52 () =
            let _prev_found_53 = false  in
            match ((Enum.peek e1_49), (Enum.peek e2_50)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_54,None ) -> (Enum.junk e1_49; `Left x_54)
            | (None ,Some y_55) -> (Enum.junk e2_50; `Right y_55)
            | (Some x_56,Some y_57) ->
                let k1_58 = k1_47 x_56  in
                let k2_59 = k2_48 y_57  in
                (match cmp_46 k1_58 k2_59 with
                 | 0 ->
                     (Enum.junk e1_49; Enum.junk e2_50; `Both (x_56, y_57))
                 | n_60 when n_60 < 0 -> (Enum.junk e1_49; `Left x_56)
                 | _ -> (Enum.junk e2_50; `Right y_57))
             in
          Enum.from next_52

let join_full_by_key cmp k = join_full_by cmp k k

let join_inner_multi_by =
fun cmp_61  ->
  fun k1_62  ->
    fun k2_63  ->
      fun e1_64  ->
        fun e2_65  ->
          let _found_66 = Pervasives.ref false  in
          let rec next_67 () =
            let _prev_found_68 = false  in
            match ((Enum.peek e1_64), (Enum.peek e2_65)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_69,None ) -> (Enum.junk e1_64; next_67 ())
            | (None ,Some y_70) ->
                (Enum.junk e2_65; Pervasives.raise Enum.No_more_elements)
            | (Some x_71,Some y_72) ->
                let k1_73 = k1_62 x_71  in
                let k2_74 = k2_63 y_72  in
                (match cmp_61 k1_73 k2_74 with
                 | 0 -> ((); Enum.junk e2_65; (x_71, y_72))
                 | n_75 when n_75 < 0 -> (Enum.junk e1_64; next_67 ())
                 | _ -> (Enum.junk e2_65; next_67 ()))
             in
          Enum.from next_67

let join_inner_multi_by_key cmp k = join_inner_multi_by cmp k k

let join_left_multi_by =
fun cmp_76  ->
  fun k1_77  ->
    fun k2_78  ->
      fun e1_79  ->
        fun e2_80  ->
          let _found_81 = Pervasives.ref false  in
          let rec next_82 () =
            let _prev_found_84 =
              let prev_83 = ! _found_81  in _found_81 := false; prev_83  in
            match ((Enum.peek e1_79), (Enum.peek e2_80)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_85,None ) ->
                (Enum.junk e1_79;
                 if Pervasives.not _prev_found_84
                 then (x_85, None)
                 else next_82 ())
            | (None ,Some y_86) ->
                (Enum.junk e2_80; Pervasives.raise Enum.No_more_elements)
            | (Some x_87,Some y_88) ->
                let k1_89 = k1_77 x_87  in
                let k2_90 = k2_78 y_88  in
                (match cmp_76 k1_89 k2_90 with
                 | 0 ->
                     (_found_81 := true; Enum.junk e2_80; (x_87, (Some y_88)))
                 | n_91 when n_91 < 0 ->
                     (Enum.junk e1_79;
                      if Pervasives.not _prev_found_84
                      then (x_87, None)
                      else next_82 ())
                 | _ -> (Enum.junk e2_80; next_82 ()))
             in
          Enum.from next_82

let join_left_multi_by_key cmp k = join_left_multi_by cmp k k

let join_right_multi_by =
fun cmp_92  ->
  fun k1_93  ->
    fun k2_94  ->
      fun e1_95  ->
        fun e2_96  ->
          let _found_97 = Pervasives.ref false  in
          let rec next_98 () =
            let _prev_found_99 = false  in
            match ((Enum.peek e1_95), (Enum.peek e2_96)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_100,None ) -> (Enum.junk e1_95; next_98 ())
            | (None ,Some y_101) -> (Enum.junk e2_96; (None, y_101))
            | (Some x_102,Some y_103) ->
                let k1_104 = k1_93 x_102  in
                let k2_105 = k2_94 y_103  in
                (match cmp_92 k1_104 k2_105 with
                 | 0 -> ((); Enum.junk e2_96; ((Some x_102), y_103))
                 | n_106 when n_106 < 0 -> (Enum.junk e1_95; next_98 ())
                 | _ -> (Enum.junk e2_96; (None, y_103)))
             in
          Enum.from next_98

let join_right_multi_by_key cmp k = join_right_multi_by cmp k k

let join_full_multi_by =
fun cmp_107  ->
  fun k1_108  ->
    fun k2_109  ->
      fun e1_110  ->
        fun e2_111  ->
          let _found_112 = Pervasives.ref false  in
          let rec next_113 () =
            let _prev_found_115 =
              let prev_114 = ! _found_112  in _found_112 := false; prev_114
               in
            match ((Enum.peek e1_110), (Enum.peek e2_111)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_116,None ) ->
                (Enum.junk e1_110;
                 if Pervasives.not _prev_found_115
                 then `Left x_116
                 else next_113 ())
            | (None ,Some y_117) -> (Enum.junk e2_111; `Right y_117)
            | (Some x_118,Some y_119) ->
                let k1_120 = k1_108 x_118  in
                let k2_121 = k2_109 y_119  in
                (match cmp_107 k1_120 k2_121 with
                 | 0 ->
                     (_found_112 := true;
                      Enum.junk e2_111;
                      `Both (x_118, y_119))
                 | n_122 when n_122 < 0 ->
                     (Enum.junk e1_110;
                      if Pervasives.not _prev_found_115
                      then `Left x_118
                      else next_113 ())
                 | _ -> (Enum.junk e2_111; `Right y_119))
             in
          Enum.from next_113

let join_full_multi_by_key cmp k = join_full_multi_by cmp k k

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
                 (Enum.junk e1_124;
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
        let _prev_found_141 = false  in
        match ((Enum.peek e1_137), (Enum.peek e2_138)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_142,None ) ->
            (Enum.junk e1_137;
             ((Pervasives.fst x_142), (Pervasives.snd x_142), None))
        | (None ,Some y_143) ->
            (Enum.junk e2_138; Pervasives.raise Enum.No_more_elements)
        | (Some x_144,Some y_145) ->
            let k1_146 = Pervasives.fst x_144  in
            let k2_147 = Pervasives.fst y_145  in
            (match cmp_136 k1_146 k2_147 with
             | 0 ->
                 (Enum.junk e1_137;
                  Enum.junk e2_138;
                  (k1_146, (Pervasives.snd x_144),
                    (Some (Pervasives.snd y_145))))
             | n_148 when n_148 < 0 ->
                 (Enum.junk e1_137; (k1_146, (Pervasives.snd x_144), None))
             | _ -> (Enum.junk e2_138; next_140 ()))
         in
      Enum.from next_140

let join_assoc_right =
fun cmp_149  ->
  fun e1_150  ->
    fun e2_151  ->
      let _found_152 = Pervasives.ref false  in
      let rec next_153 () =
        let _prev_found_154 = false  in
        match ((Enum.peek e1_150), (Enum.peek e2_151)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_155,None ) -> (Enum.junk e1_150; next_153 ())
        | (None ,Some y_156) ->
            (Enum.junk e2_151;
             ((Pervasives.fst y_156), None, (Pervasives.snd y_156)))
        | (Some x_157,Some y_158) ->
            let k1_159 = Pervasives.fst x_157  in
            let k2_160 = Pervasives.fst y_158  in
            (match cmp_149 k1_159 k2_160 with
             | 0 ->
                 (Enum.junk e1_150;
                  Enum.junk e2_151;
                  (k1_159, (Some (Pervasives.snd x_157)),
                    (Pervasives.snd y_158)))
             | n_161 when n_161 < 0 -> (Enum.junk e1_150; next_153 ())
             | _ ->
                 (Enum.junk e2_151; (k2_160, None, (Pervasives.snd y_158))))
         in
      Enum.from next_153

let join_assoc_full =
fun cmp_162  ->
  fun e1_163  ->
    fun e2_164  ->
      let _found_165 = Pervasives.ref false  in
      let rec next_166 () =
        let _prev_found_167 = false  in
        match ((Enum.peek e1_163), (Enum.peek e2_164)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_168,None ) ->
            (Enum.junk e1_163;
             ((Pervasives.fst x_168), (`Left (Pervasives.snd x_168))))
        | (None ,Some y_169) ->
            (Enum.junk e2_164;
             ((Pervasives.fst y_169), (`Right (Pervasives.snd y_169))))
        | (Some x_170,Some y_171) ->
            let k1_172 = Pervasives.fst x_170  in
            let k2_173 = Pervasives.fst y_171  in
            (match cmp_162 k1_172 k2_173 with
             | 0 ->
                 (Enum.junk e1_163;
                  Enum.junk e2_164;
                  (k1_172,
                    (`Both ((Pervasives.snd x_170), (Pervasives.snd y_171)))))
             | n_174 when n_174 < 0 ->
                 (Enum.junk e1_163; (k1_172, (`Left (Pervasives.snd x_170))))
             | _ ->
                 (Enum.junk e2_164; (k2_173, (`Right (Pervasives.snd y_171)))))
         in
      Enum.from next_166

let join_assoc_inner_multi =
fun cmp_175  ->
  fun e1_176  ->
    fun e2_177  ->
      let _found_178 = Pervasives.ref false  in
      let rec next_179 () =
        let _prev_found_180 = false  in
        match ((Enum.peek e1_176), (Enum.peek e2_177)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_181,None ) -> (Enum.junk e1_176; next_179 ())
        | (None ,Some y_182) ->
            (Enum.junk e2_177; Pervasives.raise Enum.No_more_elements)
        | (Some x_183,Some y_184) ->
            let k1_185 = Pervasives.fst x_183  in
            let k2_186 = Pervasives.fst y_184  in
            (match cmp_175 k1_185 k2_186 with
             | 0 ->
                 (();
                  Enum.junk e2_177;
                  (k1_185, (Pervasives.snd x_183), (Pervasives.snd y_184)))
             | n_187 when n_187 < 0 -> (Enum.junk e1_176; next_179 ())
             | _ -> (Enum.junk e2_177; next_179 ()))
         in
      Enum.from next_179

let join_assoc_left_multi =
fun cmp_188  ->
  fun e1_189  ->
    fun e2_190  ->
      let _found_191 = Pervasives.ref false  in
      let rec next_192 () =
        let _prev_found_194 =
          let prev_193 = ! _found_191  in _found_191 := false; prev_193  in
        match ((Enum.peek e1_189), (Enum.peek e2_190)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_195,None ) ->
            (Enum.junk e1_189;
             if Pervasives.not _prev_found_194
             then ((Pervasives.fst x_195), (Pervasives.snd x_195), None)
             else next_192 ())
        | (None ,Some y_196) ->
            (Enum.junk e2_190; Pervasives.raise Enum.No_more_elements)
        | (Some x_197,Some y_198) ->
            let k1_199 = Pervasives.fst x_197  in
            let k2_200 = Pervasives.fst y_198  in
            (match cmp_188 k1_199 k2_200 with
             | 0 ->
                 (_found_191 := true;
                  Enum.junk e2_190;
                  (k1_199, (Pervasives.snd x_197),
                    (Some (Pervasives.snd y_198))))
             | n_201 when n_201 < 0 ->
                 (Enum.junk e1_189;
                  if Pervasives.not _prev_found_194
                  then (k1_199, (Pervasives.snd x_197), None)
                  else next_192 ())
             | _ -> (Enum.junk e2_190; next_192 ()))
         in
      Enum.from next_192

let join_assoc_right_multi =
fun cmp_202  ->
  fun e1_203  ->
    fun e2_204  ->
      let _found_205 = Pervasives.ref false  in
      let rec next_206 () =
        let _prev_found_207 = false  in
        match ((Enum.peek e1_203), (Enum.peek e2_204)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_208,None ) -> (Enum.junk e1_203; next_206 ())
        | (None ,Some y_209) ->
            (Enum.junk e2_204;
             ((Pervasives.fst y_209), None, (Pervasives.snd y_209)))
        | (Some x_210,Some y_211) ->
            let k1_212 = Pervasives.fst x_210  in
            let k2_213 = Pervasives.fst y_211  in
            (match cmp_202 k1_212 k2_213 with
             | 0 ->
                 (();
                  Enum.junk e2_204;
                  (k1_212, (Some (Pervasives.snd x_210)),
                    (Pervasives.snd y_211)))
             | n_214 when n_214 < 0 -> (Enum.junk e1_203; next_206 ())
             | _ ->
                 (Enum.junk e2_204; (k2_213, None, (Pervasives.snd y_211))))
         in
      Enum.from next_206

let join_assoc_full_multi =
fun cmp_215  ->
  fun e1_216  ->
    fun e2_217  ->
      let _found_218 = Pervasives.ref false  in
      let rec next_219 () =
        let _prev_found_221 =
          let prev_220 = ! _found_218  in _found_218 := false; prev_220  in
        match ((Enum.peek e1_216), (Enum.peek e2_217)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_222,None ) ->
            (Enum.junk e1_216;
             if Pervasives.not _prev_found_221
             then ((Pervasives.fst x_222), (`Left (Pervasives.snd x_222)))
             else next_219 ())
        | (None ,Some y_223) ->
            (Enum.junk e2_217;
             ((Pervasives.fst y_223), (`Right (Pervasives.snd y_223))))
        | (Some x_224,Some y_225) ->
            let k1_226 = Pervasives.fst x_224  in
            let k2_227 = Pervasives.fst y_225  in
            (match cmp_215 k1_226 k2_227 with
             | 0 ->
                 (_found_218 := true;
                  Enum.junk e2_217;
                  (k1_226,
                    (`Both ((Pervasives.snd x_224), (Pervasives.snd y_225)))))
             | n_228 when n_228 < 0 ->
                 (Enum.junk e1_216;
                  if Pervasives.not _prev_found_221
                  then (k1_226, (`Left (Pervasives.snd x_224)))
                  else next_219 ())
             | _ ->
                 (Enum.junk e2_217; (k2_227, (`Right (Pervasives.snd y_225)))))
         in
      Enum.from next_219

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

