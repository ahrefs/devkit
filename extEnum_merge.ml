let join_inner =
fun cmp_1  ->
  fun e1_2  ->
    fun e2_3  ->
      let _found_4 = Pervasives.ref false  in
      let rec next_5 () =
        let _prev_found_6 = false  in
        match ((Enum.peek e1_2), (Enum.peek e2_3)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_7,None ) -> (Enum.junk e1_2; next_5 ())
        | (None ,Some y_8) ->
            (Enum.junk e2_3; Pervasives.raise Enum.No_more_elements)
        | (Some x_9,Some y_10) ->
            let k1_11 = x_9  in
            let k2_12 = y_10  in
            (match cmp_1 k1_11 k2_12 with
             | 0 -> ((); Enum.junk e2_3; (x_9, y_10))
             | n_13 when n_13 < 0 -> (Enum.junk e1_2; next_5 ())
             | _ -> (Enum.junk e2_3; next_5 ()))
         in
      Enum.from next_5

let join_inner_by =
fun cmp_14  ->
  fun k1_15  ->
    fun k2_16  ->
      fun e1_17  ->
        fun e2_18  ->
          let _found_19 = Pervasives.ref false  in
          let rec next_20 () =
            let _prev_found_21 = false  in
            match ((Enum.peek e1_17), (Enum.peek e2_18)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_22,None ) -> (Enum.junk e1_17; next_20 ())
            | (None ,Some y_23) ->
                (Enum.junk e2_18; Pervasives.raise Enum.No_more_elements)
            | (Some x_24,Some y_25) ->
                let k1_26 = k1_15 x_24  in
                let k2_27 = k2_16 y_25  in
                (match cmp_14 k1_26 k2_27 with
                 | 0 -> ((); Enum.junk e2_18; (x_24, y_25))
                 | n_28 when n_28 < 0 -> (Enum.junk e1_17; next_20 ())
                 | _ -> (Enum.junk e2_18; next_20 ()))
             in
          Enum.from next_20

let join_inner_by_key cmp k = join_inner_by cmp k k

let join_left =
fun cmp_29  ->
  fun e1_30  ->
    fun e2_31  ->
      let _found_32 = Pervasives.ref false  in
      let rec next_33 () =
        let _prev_found_35 =
          let prev_34 = ! _found_32  in _found_32 := false; prev_34  in
        match ((Enum.peek e1_30), (Enum.peek e2_31)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_36,None ) ->
            (Enum.junk e1_30;
             if Pervasives.not _prev_found_35
             then (x_36, None)
             else next_33 ())
        | (None ,Some y_37) ->
            (Enum.junk e2_31; Pervasives.raise Enum.No_more_elements)
        | (Some x_38,Some y_39) ->
            let k1_40 = x_38  in
            let k2_41 = y_39  in
            (match cmp_29 k1_40 k2_41 with
             | 0 -> (_found_32 := true; Enum.junk e2_31; (x_38, (Some y_39)))
             | n_42 when n_42 < 0 ->
                 (Enum.junk e1_30;
                  if Pervasives.not _prev_found_35
                  then (x_38, None)
                  else next_33 ())
             | _ -> (Enum.junk e2_31; next_33 ()))
         in
      Enum.from next_33

let join_left_by =
fun cmp_43  ->
  fun k1_44  ->
    fun k2_45  ->
      fun e1_46  ->
        fun e2_47  ->
          let _found_48 = Pervasives.ref false  in
          let rec next_49 () =
            let _prev_found_51 =
              let prev_50 = ! _found_48  in _found_48 := false; prev_50  in
            match ((Enum.peek e1_46), (Enum.peek e2_47)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_52,None ) ->
                (Enum.junk e1_46;
                 if Pervasives.not _prev_found_51
                 then (x_52, None)
                 else next_49 ())
            | (None ,Some y_53) ->
                (Enum.junk e2_47; Pervasives.raise Enum.No_more_elements)
            | (Some x_54,Some y_55) ->
                let k1_56 = k1_44 x_54  in
                let k2_57 = k2_45 y_55  in
                (match cmp_43 k1_56 k2_57 with
                 | 0 ->
                     (_found_48 := true; Enum.junk e2_47; (x_54, (Some y_55)))
                 | n_58 when n_58 < 0 ->
                     (Enum.junk e1_46;
                      if Pervasives.not _prev_found_51
                      then (x_54, None)
                      else next_49 ())
                 | _ -> (Enum.junk e2_47; next_49 ()))
             in
          Enum.from next_49

let join_left_by_key cmp k = join_left_by cmp k k

let join_right =
fun cmp_59  ->
  fun e1_60  ->
    fun e2_61  ->
      let _found_62 = Pervasives.ref false  in
      let rec next_63 () =
        let _prev_found_64 = false  in
        match ((Enum.peek e1_60), (Enum.peek e2_61)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_65,None ) -> (Enum.junk e1_60; next_63 ())
        | (None ,Some y_66) -> (Enum.junk e2_61; (None, y_66))
        | (Some x_67,Some y_68) ->
            let k1_69 = x_67  in
            let k2_70 = y_68  in
            (match cmp_59 k1_69 k2_70 with
             | 0 -> ((); Enum.junk e2_61; ((Some x_67), y_68))
             | n_71 when n_71 < 0 -> (Enum.junk e1_60; next_63 ())
             | _ -> (Enum.junk e2_61; (None, y_68)))
         in
      Enum.from next_63

let join_right_by =
fun cmp_72  ->
  fun k1_73  ->
    fun k2_74  ->
      fun e1_75  ->
        fun e2_76  ->
          let _found_77 = Pervasives.ref false  in
          let rec next_78 () =
            let _prev_found_79 = false  in
            match ((Enum.peek e1_75), (Enum.peek e2_76)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_80,None ) -> (Enum.junk e1_75; next_78 ())
            | (None ,Some y_81) -> (Enum.junk e2_76; (None, y_81))
            | (Some x_82,Some y_83) ->
                let k1_84 = k1_73 x_82  in
                let k2_85 = k2_74 y_83  in
                (match cmp_72 k1_84 k2_85 with
                 | 0 -> ((); Enum.junk e2_76; ((Some x_82), y_83))
                 | n_86 when n_86 < 0 -> (Enum.junk e1_75; next_78 ())
                 | _ -> (Enum.junk e2_76; (None, y_83)))
             in
          Enum.from next_78

let join_right_by_key cmp k = join_right_by cmp k k

let join_full =
fun cmp_87  ->
  fun e1_88  ->
    fun e2_89  ->
      let _found_90 = Pervasives.ref false  in
      let rec next_91 () =
        let _prev_found_93 =
          let prev_92 = ! _found_90  in _found_90 := false; prev_92  in
        match ((Enum.peek e1_88), (Enum.peek e2_89)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_94,None ) ->
            (Enum.junk e1_88;
             if Pervasives.not _prev_found_93
             then ((Some x_94), None)
             else next_91 ())
        | (None ,Some y_95) -> (Enum.junk e2_89; (None, (Some y_95)))
        | (Some x_96,Some y_97) ->
            let k1_98 = x_96  in
            let k2_99 = y_97  in
            (match cmp_87 k1_98 k2_99 with
             | 0 ->
                 (_found_90 := true;
                  Enum.junk e2_89;
                  ((Some x_96), (Some y_97)))
             | n_100 when n_100 < 0 ->
                 (Enum.junk e1_88;
                  if Pervasives.not _prev_found_93
                  then ((Some x_96), None)
                  else next_91 ())
             | _ -> (Enum.junk e2_89; (None, (Some y_97))))
         in
      Enum.from next_91

let join_full_by =
fun cmp_101  ->
  fun k1_102  ->
    fun k2_103  ->
      fun e1_104  ->
        fun e2_105  ->
          let _found_106 = Pervasives.ref false  in
          let rec next_107 () =
            let _prev_found_109 =
              let prev_108 = ! _found_106  in _found_106 := false; prev_108
               in
            match ((Enum.peek e1_104), (Enum.peek e2_105)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_110,None ) ->
                (Enum.junk e1_104;
                 if Pervasives.not _prev_found_109
                 then ((Some x_110), None)
                 else next_107 ())
            | (None ,Some y_111) -> (Enum.junk e2_105; (None, (Some y_111)))
            | (Some x_112,Some y_113) ->
                let k1_114 = k1_102 x_112  in
                let k2_115 = k2_103 y_113  in
                (match cmp_101 k1_114 k2_115 with
                 | 0 ->
                     (_found_106 := true;
                      Enum.junk e2_105;
                      ((Some x_112), (Some y_113)))
                 | n_116 when n_116 < 0 ->
                     (Enum.junk e1_104;
                      if Pervasives.not _prev_found_109
                      then ((Some x_112), None)
                      else next_107 ())
                 | _ -> (Enum.junk e2_105; (None, (Some y_113))))
             in
          Enum.from next_107

let join_full_by_key cmp k = join_full_by cmp k k

let join_inner_uniq =
fun cmp_117  ->
  fun e1_118  ->
    fun e2_119  ->
      let _found_120 = Pervasives.ref false  in
      let rec next_121 () =
        let _prev_found_122 = false  in
        match ((Enum.peek e1_118), (Enum.peek e2_119)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_123,None ) -> (Enum.junk e1_118; next_121 ())
        | (None ,Some y_124) ->
            (Enum.junk e2_119; Pervasives.raise Enum.No_more_elements)
        | (Some x_125,Some y_126) ->
            let k1_127 = x_125  in
            let k2_128 = y_126  in
            (match cmp_117 k1_127 k2_128 with
             | 0 -> (Enum.junk e1_118; Enum.junk e2_119; (x_125, y_126))
             | n_129 when n_129 < 0 -> (Enum.junk e1_118; next_121 ())
             | _ -> (Enum.junk e2_119; next_121 ()))
         in
      Enum.from next_121

let join_inner_uniq_by =
fun cmp_130  ->
  fun k1_131  ->
    fun k2_132  ->
      fun e1_133  ->
        fun e2_134  ->
          let _found_135 = Pervasives.ref false  in
          let rec next_136 () =
            let _prev_found_137 = false  in
            match ((Enum.peek e1_133), (Enum.peek e2_134)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_138,None ) -> (Enum.junk e1_133; next_136 ())
            | (None ,Some y_139) ->
                (Enum.junk e2_134; Pervasives.raise Enum.No_more_elements)
            | (Some x_140,Some y_141) ->
                let k1_142 = k1_131 x_140  in
                let k2_143 = k2_132 y_141  in
                (match cmp_130 k1_142 k2_143 with
                 | 0 -> (Enum.junk e1_133; Enum.junk e2_134; (x_140, y_141))
                 | n_144 when n_144 < 0 -> (Enum.junk e1_133; next_136 ())
                 | _ -> (Enum.junk e2_134; next_136 ()))
             in
          Enum.from next_136

let join_inner_uniq_by_key cmp k = join_inner_uniq_by cmp k k

let join_left_uniq =
fun cmp_145  ->
  fun e1_146  ->
    fun e2_147  ->
      let _found_148 = Pervasives.ref false  in
      let rec next_149 () =
        let _prev_found_150 = false  in
        match ((Enum.peek e1_146), (Enum.peek e2_147)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_151,None ) -> (Enum.junk e1_146; (x_151, None))
        | (None ,Some y_152) ->
            (Enum.junk e2_147; Pervasives.raise Enum.No_more_elements)
        | (Some x_153,Some y_154) ->
            let k1_155 = x_153  in
            let k2_156 = y_154  in
            (match cmp_145 k1_155 k2_156 with
             | 0 ->
                 (Enum.junk e1_146; Enum.junk e2_147; (x_153, (Some y_154)))
             | n_157 when n_157 < 0 -> (Enum.junk e1_146; (x_153, None))
             | _ -> (Enum.junk e2_147; next_149 ()))
         in
      Enum.from next_149

let join_left_uniq_by =
fun cmp_158  ->
  fun k1_159  ->
    fun k2_160  ->
      fun e1_161  ->
        fun e2_162  ->
          let _found_163 = Pervasives.ref false  in
          let rec next_164 () =
            let _prev_found_165 = false  in
            match ((Enum.peek e1_161), (Enum.peek e2_162)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_166,None ) -> (Enum.junk e1_161; (x_166, None))
            | (None ,Some y_167) ->
                (Enum.junk e2_162; Pervasives.raise Enum.No_more_elements)
            | (Some x_168,Some y_169) ->
                let k1_170 = k1_159 x_168  in
                let k2_171 = k2_160 y_169  in
                (match cmp_158 k1_170 k2_171 with
                 | 0 ->
                     (Enum.junk e1_161;
                      Enum.junk e2_162;
                      (x_168, (Some y_169)))
                 | n_172 when n_172 < 0 -> (Enum.junk e1_161; (x_168, None))
                 | _ -> (Enum.junk e2_162; next_164 ()))
             in
          Enum.from next_164

let join_left_uniq_by_key cmp k = join_left_uniq_by cmp k k

let join_right_uniq =
fun cmp_173  ->
  fun e1_174  ->
    fun e2_175  ->
      let _found_176 = Pervasives.ref false  in
      let rec next_177 () =
        let _prev_found_178 = false  in
        match ((Enum.peek e1_174), (Enum.peek e2_175)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_179,None ) -> (Enum.junk e1_174; next_177 ())
        | (None ,Some y_180) -> (Enum.junk e2_175; (None, y_180))
        | (Some x_181,Some y_182) ->
            let k1_183 = x_181  in
            let k2_184 = y_182  in
            (match cmp_173 k1_183 k2_184 with
             | 0 ->
                 (Enum.junk e1_174; Enum.junk e2_175; ((Some x_181), y_182))
             | n_185 when n_185 < 0 -> (Enum.junk e1_174; next_177 ())
             | _ -> (Enum.junk e2_175; (None, y_182)))
         in
      Enum.from next_177

let join_right_uniq_by =
fun cmp_186  ->
  fun k1_187  ->
    fun k2_188  ->
      fun e1_189  ->
        fun e2_190  ->
          let _found_191 = Pervasives.ref false  in
          let rec next_192 () =
            let _prev_found_193 = false  in
            match ((Enum.peek e1_189), (Enum.peek e2_190)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_194,None ) -> (Enum.junk e1_189; next_192 ())
            | (None ,Some y_195) -> (Enum.junk e2_190; (None, y_195))
            | (Some x_196,Some y_197) ->
                let k1_198 = k1_187 x_196  in
                let k2_199 = k2_188 y_197  in
                (match cmp_186 k1_198 k2_199 with
                 | 0 ->
                     (Enum.junk e1_189;
                      Enum.junk e2_190;
                      ((Some x_196), y_197))
                 | n_200 when n_200 < 0 -> (Enum.junk e1_189; next_192 ())
                 | _ -> (Enum.junk e2_190; (None, y_197)))
             in
          Enum.from next_192

let join_right_uniq_by_key cmp k = join_right_uniq_by cmp k k

let join_full_uniq =
fun cmp_201  ->
  fun e1_202  ->
    fun e2_203  ->
      let _found_204 = Pervasives.ref false  in
      let rec next_205 () =
        let _prev_found_206 = false  in
        match ((Enum.peek e1_202), (Enum.peek e2_203)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_207,None ) -> (Enum.junk e1_202; ((Some x_207), None))
        | (None ,Some y_208) -> (Enum.junk e2_203; (None, (Some y_208)))
        | (Some x_209,Some y_210) ->
            let k1_211 = x_209  in
            let k2_212 = y_210  in
            (match cmp_201 k1_211 k2_212 with
             | 0 ->
                 (Enum.junk e1_202;
                  Enum.junk e2_203;
                  ((Some x_209), (Some y_210)))
             | n_213 when n_213 < 0 ->
                 (Enum.junk e1_202; ((Some x_209), None))
             | _ -> (Enum.junk e2_203; (None, (Some y_210))))
         in
      Enum.from next_205

let join_full_uniq_by =
fun cmp_214  ->
  fun k1_215  ->
    fun k2_216  ->
      fun e1_217  ->
        fun e2_218  ->
          let _found_219 = Pervasives.ref false  in
          let rec next_220 () =
            let _prev_found_221 = false  in
            match ((Enum.peek e1_217), (Enum.peek e2_218)) with
            | (None ,None ) -> Pervasives.raise Enum.No_more_elements
            | (Some x_222,None ) -> (Enum.junk e1_217; ((Some x_222), None))
            | (None ,Some y_223) -> (Enum.junk e2_218; (None, (Some y_223)))
            | (Some x_224,Some y_225) ->
                let k1_226 = k1_215 x_224  in
                let k2_227 = k2_216 y_225  in
                (match cmp_214 k1_226 k2_227 with
                 | 0 ->
                     (Enum.junk e1_217;
                      Enum.junk e2_218;
                      ((Some x_224), (Some y_225)))
                 | n_228 when n_228 < 0 ->
                     (Enum.junk e1_217; ((Some x_224), None))
                 | _ -> (Enum.junk e2_218; (None, (Some y_225))))
             in
          Enum.from next_220

let join_full_uniq_by_key cmp k = join_full_uniq_by cmp k k

let join_assoc_inner =
fun cmp_229  ->
  fun e1_230  ->
    fun e2_231  ->
      let _found_232 = Pervasives.ref false  in
      let rec next_233 () =
        let _prev_found_234 = false  in
        match ((Enum.peek e1_230), (Enum.peek e2_231)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_235,None ) -> (Enum.junk e1_230; next_233 ())
        | (None ,Some y_236) ->
            (Enum.junk e2_231; Pervasives.raise Enum.No_more_elements)
        | (Some x_237,Some y_238) ->
            let k1_239 = Pervasives.fst x_237  in
            let k2_240 = Pervasives.fst y_238  in
            (match cmp_229 k1_239 k2_240 with
             | 0 ->
                 (();
                  Enum.junk e2_231;
                  (k1_239, (Pervasives.snd x_237), (Pervasives.snd y_238)))
             | n_241 when n_241 < 0 -> (Enum.junk e1_230; next_233 ())
             | _ -> (Enum.junk e2_231; next_233 ()))
         in
      Enum.from next_233

let join_assoc_left =
fun cmp_242  ->
  fun e1_243  ->
    fun e2_244  ->
      let _found_245 = Pervasives.ref false  in
      let rec next_246 () =
        let _prev_found_248 =
          let prev_247 = ! _found_245  in _found_245 := false; prev_247  in
        match ((Enum.peek e1_243), (Enum.peek e2_244)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_249,None ) ->
            (Enum.junk e1_243;
             if Pervasives.not _prev_found_248
             then ((Pervasives.fst x_249), (Pervasives.snd x_249), None)
             else next_246 ())
        | (None ,Some y_250) ->
            (Enum.junk e2_244; Pervasives.raise Enum.No_more_elements)
        | (Some x_251,Some y_252) ->
            let k1_253 = Pervasives.fst x_251  in
            let k2_254 = Pervasives.fst y_252  in
            (match cmp_242 k1_253 k2_254 with
             | 0 ->
                 (_found_245 := true;
                  Enum.junk e2_244;
                  (k1_253, (Pervasives.snd x_251),
                    (Some (Pervasives.snd y_252))))
             | n_255 when n_255 < 0 ->
                 (Enum.junk e1_243;
                  if Pervasives.not _prev_found_248
                  then (k1_253, (Pervasives.snd x_251), None)
                  else next_246 ())
             | _ -> (Enum.junk e2_244; next_246 ()))
         in
      Enum.from next_246

let join_assoc_right =
fun cmp_256  ->
  fun e1_257  ->
    fun e2_258  ->
      let _found_259 = Pervasives.ref false  in
      let rec next_260 () =
        let _prev_found_261 = false  in
        match ((Enum.peek e1_257), (Enum.peek e2_258)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_262,None ) -> (Enum.junk e1_257; next_260 ())
        | (None ,Some y_263) ->
            (Enum.junk e2_258;
             ((Pervasives.fst y_263), None, (Pervasives.snd y_263)))
        | (Some x_264,Some y_265) ->
            let k1_266 = Pervasives.fst x_264  in
            let k2_267 = Pervasives.fst y_265  in
            (match cmp_256 k1_266 k2_267 with
             | 0 ->
                 (();
                  Enum.junk e2_258;
                  (k1_266, (Some (Pervasives.snd x_264)),
                    (Pervasives.snd y_265)))
             | n_268 when n_268 < 0 -> (Enum.junk e1_257; next_260 ())
             | _ ->
                 (Enum.junk e2_258; (k2_267, None, (Pervasives.snd y_265))))
         in
      Enum.from next_260

let join_assoc_full =
fun cmp_269  ->
  fun e1_270  ->
    fun e2_271  ->
      let _found_272 = Pervasives.ref false  in
      let rec next_273 () =
        let _prev_found_275 =
          let prev_274 = ! _found_272  in _found_272 := false; prev_274  in
        match ((Enum.peek e1_270), (Enum.peek e2_271)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_276,None ) ->
            (Enum.junk e1_270;
             if Pervasives.not _prev_found_275
             then
               ((Pervasives.fst x_276), (Some (Pervasives.snd x_276)), None)
             else next_273 ())
        | (None ,Some y_277) ->
            (Enum.junk e2_271;
             ((Pervasives.fst y_277), None, (Some (Pervasives.snd y_277))))
        | (Some x_278,Some y_279) ->
            let k1_280 = Pervasives.fst x_278  in
            let k2_281 = Pervasives.fst y_279  in
            (match cmp_269 k1_280 k2_281 with
             | 0 ->
                 (_found_272 := true;
                  Enum.junk e2_271;
                  (k1_280, (Some (Pervasives.snd x_278)),
                    (Some (Pervasives.snd y_279))))
             | n_282 when n_282 < 0 ->
                 (Enum.junk e1_270;
                  if Pervasives.not _prev_found_275
                  then (k1_280, (Some (Pervasives.snd x_278)), None)
                  else next_273 ())
             | _ ->
                 (Enum.junk e2_271;
                  (k2_281, None, (Some (Pervasives.snd y_279)))))
         in
      Enum.from next_273

let join_assoc_inner_uniq =
fun cmp_283  ->
  fun e1_284  ->
    fun e2_285  ->
      let _found_286 = Pervasives.ref false  in
      let rec next_287 () =
        let _prev_found_288 = false  in
        match ((Enum.peek e1_284), (Enum.peek e2_285)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_289,None ) -> (Enum.junk e1_284; next_287 ())
        | (None ,Some y_290) ->
            (Enum.junk e2_285; Pervasives.raise Enum.No_more_elements)
        | (Some x_291,Some y_292) ->
            let k1_293 = Pervasives.fst x_291  in
            let k2_294 = Pervasives.fst y_292  in
            (match cmp_283 k1_293 k2_294 with
             | 0 ->
                 (Enum.junk e1_284;
                  Enum.junk e2_285;
                  (k1_293, (Pervasives.snd x_291), (Pervasives.snd y_292)))
             | n_295 when n_295 < 0 -> (Enum.junk e1_284; next_287 ())
             | _ -> (Enum.junk e2_285; next_287 ()))
         in
      Enum.from next_287

let join_assoc_left_uniq =
fun cmp_296  ->
  fun e1_297  ->
    fun e2_298  ->
      let _found_299 = Pervasives.ref false  in
      let rec next_300 () =
        let _prev_found_301 = false  in
        match ((Enum.peek e1_297), (Enum.peek e2_298)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_302,None ) ->
            (Enum.junk e1_297;
             ((Pervasives.fst x_302), (Pervasives.snd x_302), None))
        | (None ,Some y_303) ->
            (Enum.junk e2_298; Pervasives.raise Enum.No_more_elements)
        | (Some x_304,Some y_305) ->
            let k1_306 = Pervasives.fst x_304  in
            let k2_307 = Pervasives.fst y_305  in
            (match cmp_296 k1_306 k2_307 with
             | 0 ->
                 (Enum.junk e1_297;
                  Enum.junk e2_298;
                  (k1_306, (Pervasives.snd x_304),
                    (Some (Pervasives.snd y_305))))
             | n_308 when n_308 < 0 ->
                 (Enum.junk e1_297; (k1_306, (Pervasives.snd x_304), None))
             | _ -> (Enum.junk e2_298; next_300 ()))
         in
      Enum.from next_300

let join_assoc_right_uniq =
fun cmp_309  ->
  fun e1_310  ->
    fun e2_311  ->
      let _found_312 = Pervasives.ref false  in
      let rec next_313 () =
        let _prev_found_314 = false  in
        match ((Enum.peek e1_310), (Enum.peek e2_311)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_315,None ) -> (Enum.junk e1_310; next_313 ())
        | (None ,Some y_316) ->
            (Enum.junk e2_311;
             ((Pervasives.fst y_316), None, (Pervasives.snd y_316)))
        | (Some x_317,Some y_318) ->
            let k1_319 = Pervasives.fst x_317  in
            let k2_320 = Pervasives.fst y_318  in
            (match cmp_309 k1_319 k2_320 with
             | 0 ->
                 (Enum.junk e1_310;
                  Enum.junk e2_311;
                  (k1_319, (Some (Pervasives.snd x_317)),
                    (Pervasives.snd y_318)))
             | n_321 when n_321 < 0 -> (Enum.junk e1_310; next_313 ())
             | _ ->
                 (Enum.junk e2_311; (k2_320, None, (Pervasives.snd y_318))))
         in
      Enum.from next_313

let join_assoc_full_uniq =
fun cmp_322  ->
  fun e1_323  ->
    fun e2_324  ->
      let _found_325 = Pervasives.ref false  in
      let rec next_326 () =
        let _prev_found_327 = false  in
        match ((Enum.peek e1_323), (Enum.peek e2_324)) with
        | (None ,None ) -> Pervasives.raise Enum.No_more_elements
        | (Some x_328,None ) ->
            (Enum.junk e1_323;
             ((Pervasives.fst x_328), (Some (Pervasives.snd x_328)), None))
        | (None ,Some y_329) ->
            (Enum.junk e2_324;
             ((Pervasives.fst y_329), None, (Some (Pervasives.snd y_329))))
        | (Some x_330,Some y_331) ->
            let k1_332 = Pervasives.fst x_330  in
            let k2_333 = Pervasives.fst y_331  in
            (match cmp_322 k1_332 k2_333 with
             | 0 ->
                 (Enum.junk e1_323;
                  Enum.junk e2_324;
                  (k1_332, (Some (Pervasives.snd x_330)),
                    (Some (Pervasives.snd y_331))))
             | n_334 when n_334 < 0 ->
                 (Enum.junk e1_323;
                  (k1_332, (Some (Pervasives.snd x_330)), None))
             | _ ->
                 (Enum.junk e2_324;
                  (k2_333, None, (Some (Pervasives.snd y_331)))))
         in
      Enum.from next_326

