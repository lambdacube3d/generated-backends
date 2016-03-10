{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Prelude (($),Num (..),return)
import Language
import PrettyCpp

enumConversions = do
  procedure "inputType" ["t" :@ SmartPtr "InputType"] (Enum "Type") $ do
    switch ("t"~>"tag") $ do
      case_ (nsPat ["InputType","tag","Float"]) $ return_ $ ns ["Type","FLOAT"]
      case_ (nsPat ["InputType","tag","V2F"])   $ return_ $ ns ["Type","FLOAT_VEC2"]
      case_ (nsPat ["InputType","tag","V3F"])   $ return_ $ ns ["Type","FLOAT_VEC3"]
      case_ (nsPat ["InputType","tag","V4F"])   $ return_ $ ns ["Type","FLOAT_VEC4"]
      case_ (nsPat ["InputType","tag","M22F"])  $ return_ $ ns ["Type","FLOAT_MAT2"]
      case_ (nsPat ["InputType","tag","M33F"])  $ return_ $ ns ["Type","FLOAT_MAT3"]
      case_ (nsPat ["InputType","tag","M44F"])  $ return_ $ ns ["Type","FLOAT_MAT4"]
    throw "illegal input type"

  procedure "primitiveMode" ["p" :@ "Primitive"] Int $ do
    switch "p" $ do
      case_ (nsPat ["Primitive","TriangleStrip"]) $ return_ "GL_TRIANGLE_STRIP"
      case_ (nsPat ["Primitive","TriangleList"])  $ return_ "GL_TRIANGLES"
      case_ (nsPat ["Primitive","TriangleFan"])   $ return_ "GL_TRIANGLE_FAN"
      case_ (nsPat ["Primitive","LineStrip"])     $ return_ "GL_LINE_STRIP"
      case_ (nsPat ["Primitive","LineList"])      $ return_ "GL_LINES"
      case_ (nsPat ["Primitive","LineLoop"])      $ return_ "GL_LINE_LOOP"
      case_ (nsPat ["Primitive","PointList"])     $ return_ "GL_POINTS"

  procedure "blendingFactor" ["bf" :@ SmartPtr "BlendingFactor"] Int $ do
    switch ("bf"~>"tag") $ do
      case_ (nsPat ["BlendingFactor","tag","ConstantAlpha"]) $ return_ "GL_CONSTANT_ALPHA"
      case_ (nsPat ["BlendingFactor","tag","ConstantColor"]) $ return_ "GL_CONSTANT_COLOR"
      case_ (nsPat ["BlendingFactor","tag","DstAlpha"]) $ return_ "GL_DST_ALPHA"
      case_ (nsPat ["BlendingFactor","tag","DstColor"]) $ return_ "GL_DST_COLOR"
      case_ (nsPat ["BlendingFactor","tag","One"]) $ return_ "GL_ONE"
      case_ (nsPat ["BlendingFactor","tag","OneMinusConstantAlpha"]) $ return_ "GL_ONE_MINUS_CONSTANT_ALPHA"
      case_ (nsPat ["BlendingFactor","tag","OneMinusConstantColor"]) $ return_ "GL_ONE_MINUS_CONSTANT_COLOR"
      case_ (nsPat ["BlendingFactor","tag","OneMinusDstAlpha"]) $ return_ "GL_ONE_MINUS_DST_ALPHA"
      case_ (nsPat ["BlendingFactor","tag","OneMinusDstColor"]) $ return_ "GL_ONE_MINUS_DST_COLOR"
      case_ (nsPat ["BlendingFactor","tag","OneMinusSrcAlpha"]) $ return_ "GL_ONE_MINUS_SRC_ALPHA"
      case_ (nsPat ["BlendingFactor","tag","OneMinusSrcColor"]) $ return_ "GL_ONE_MINUS_SRC_COLOR"
      case_ (nsPat ["BlendingFactor","tag","SrcAlpha"]) $ return_ "GL_SRC_ALPHA"
      case_ (nsPat ["BlendingFactor","tag","SrcAlphaSaturate"]) $ return_ "GL_SRC_ALPHA_SATURATE"
      case_ (nsPat ["BlendingFactor","tag","SrcColor"]) $ return_ "GL_SRC_COLOR"
      case_ (nsPat ["BlendingFactor","tag","Zero"]) $ return_ "GL_ZERO"
    throw "illegal blending factor"

  procedure "blendEquation" ["be" :@ SmartPtr "BlendEquation"] Int $ do
    switch ("be"~>"tag") $ do
      case_ (nsPat ["BlendEquation","tag","FuncAdd"]) $ return_ "GL_FUNC_ADD"
      case_ (nsPat ["BlendEquation","tag","FuncReverseSubtract"]) $ return_ "GL_FUNC_REVERSE_SUBTRACT"
      case_ (nsPat ["BlendEquation","tag","FuncSubtract"]) $ return_ "GL_FUNC_SUBTRACT"
    throw "illegal blend equation"

  procedure "comparisonFunction" ["cf" :@ SmartPtr "ComparisonFunction"] Int $ do
    switch ("cf"~>"tag") $ do
      case_ (nsPat ["ComparisonFunction","tag","Always"]) $ return_ "GL_ALWAYS"
      case_ (nsPat ["ComparisonFunction","tag","Equal"]) $ return_ "GL_EQUAL"
      case_ (nsPat ["ComparisonFunction","tag","Gequal"]) $ return_ "GL_GEQUAL"
      case_ (nsPat ["ComparisonFunction","tag","Greater"]) $ return_ "GL_GREATER"
      case_ (nsPat ["ComparisonFunction","tag","Lequal"]) $ return_ "GL_LEQUAL"
      case_ (nsPat ["ComparisonFunction","tag","Less"]) $ return_ "GL_LESS"
      case_ (nsPat ["ComparisonFunction","tag","Never"]) $ return_ "GL_NEVER"
      case_ (nsPat ["ComparisonFunction","tag","Notequal"]) $ return_ "GL_NOTEQUAL"
    throw "illegal comparison function"

  procedure "frontFace" ["ff" :@ SmartPtr "FrontFace"] Int $ do
    switch ("ff"~>"tag") $ do
      case_ (nsPat ["FrontFace","tag","CCW"]) $ return_ "GL_CCW"
      case_ (nsPat ["FrontFace","tag","CW"]) $ return_ "GL_CW"
    throw "illegal front face value"

  procedure "textureDataTypeToGLType" ["s_" :@ SmartPtr "ImageSemantic", "d_" :@ SmartPtr "TextureDataType"] Int $ do
    switch ("s_"~>"tag") $ do
      case_ (nsPat ["ImageSemantic","tag","Color"]) $ return_ "GL_RGBA"
      case_ (nsPat ["ImageSemantic","tag","Depth"]) $ return_ "GL_DEPTH_COMPONENT"
    throw "FIXME: This texture format is not yet supported"

  procedure "textureDataTypeToGLArityType" ["s_" :@ SmartPtr "ImageSemantic", "d_" :@ SmartPtr "TextureDataType"] Int $ do
    switch ("s_"~>"tag") $ do
      case_ (nsPat ["ImageSemantic","tag","Color"]) $ return_ "GL_RGBA"
      case_ (nsPat ["ImageSemantic","tag","Depth"]) $ return_ "GL_DEPTH_COMPONENT"
    throw "FIXME: This texture format is not yet supported"

  procedure "edgeMode" ["e" :@ SmartPtr "EdgeMode"] Int $ do
    switch ("e"~>"tag") $ do
      case_ (nsPat ["EdgeMode","tag","ClampToEdge"]) $ return_ "GL_CLAMP_TO_EDGE"
      case_ (nsPat ["EdgeMode","tag","Repeat"]) $ return_ "GL_REPEAT"
      case_ (nsPat ["EdgeMode","tag","MirroredRepeat"]) $ return_ "GL_MIRRORED_REPEAT"
      default_ $ throw "unsupported edge mode"

  procedure "filterMode" ["f" :@ SmartPtr "Filter"] Int $ do
    switch ("f"~>"tag") $ do
      case_ (nsPat ["Filter","tag","Nearest"]) $ return_ "GL_NEAREST"
      case_ (nsPat ["Filter","tag","Linear"]) $ return_ "GL_LINEAR"
      case_ (nsPat ["Filter","tag","NearestMipmapNearest"]) $ return_ "GL_NEAREST_MIPMAP_NEAREST"
      case_ (nsPat ["Filter","tag","NearestMipmapLinear"]) $ return_ "GL_NEAREST_MIPMAP_LINEAR"
      case_ (nsPat ["Filter","tag","LinearMipmapNearest"]) $ return_ "GL_LINEAR_MIPMAP_NEAREST"
      case_ (nsPat ["Filter","tag","LinearMipmapLinear"]) $ return_ "GL_LINEAR_MIPMAP_LINEAR"
      default_ $ throw "unsupported filter mode"

globalFunctions = do
  procedure "setUniformValue" ["i" :@ Int, "v" :@ Ref "UniformValue"] Void $ do
    switch ("v"."tag") $ do
      case_ (nsPat ["InputType","tag","Int"])    $ call "glUniform1i" ["i","v"."_int"]
      case_ (nsPat ["InputType","tag","Word"])   $ call "glUniform1i" ["i","v"."_word"]
      case_ (nsPat ["InputType","tag","Float"])  $ call "glUniform1f" ["i","v"."_float"]
      case_ (nsPat ["InputType","tag","Bool"])   $ call "glUniform1i" ["i","v"."_bool"]
      case_ (nsPat ["InputType","tag","V2I"])    $ call "glUniform2iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v2i"]
      case_ (nsPat ["InputType","tag","V2U"])    $ call "glUniform2iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v2u"]
      case_ (nsPat ["InputType","tag","V2F"])    $ call "glUniform2fv" ["i",1,cast (Ptr Float) $ addr $ "v"."_v2f"]
      case_ (nsPat ["InputType","tag","V2B"])    $ call "glUniform2iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v2b"]

      case_ (nsPat ["InputType","tag","V3I"])    $ call "glUniform3iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v3i"]
      case_ (nsPat ["InputType","tag","V3U"])    $ call "glUniform3iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v3u"]
      case_ (nsPat ["InputType","tag","V3F"])    $ call "glUniform3fv" ["i",1,cast (Ptr Float) $ addr $ "v"."_v3f"]
      case_ (nsPat ["InputType","tag","V3B"])    $ call "glUniform3iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v3b"]

      case_ (nsPat ["InputType","tag","V4I"])    $ call "glUniform4iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v4i"]
      case_ (nsPat ["InputType","tag","V4U"])    $ call "glUniform4iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v4u"]
      case_ (nsPat ["InputType","tag","V4F"])    $ call "glUniform4fv" ["i",1,cast (Ptr Float) $ addr $ "v"."_v4f"]
      case_ (nsPat ["InputType","tag","V4B"])    $ call "glUniform4iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v4b"]

      case_ (nsPat ["InputType","tag","M22F"])   $ call "glUniformMatrix2fv" ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m22f"]
      case_ (nsPat ["InputType","tag","M33F"])   $ call "glUniformMatrix3fv" ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m33f"]
      case_ (nsPat ["InputType","tag","M44F"])   $ call "glUniformMatrix4fv" ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m44f"]

  procedure "setStream" ["i" :@ Int, "s" :@ Ref "Stream"] Void $ do
    if_ ("s"."isArray") $ do
      then_ $ do
        call "glBindBuffer" ["GL_ARRAY_BUFFER","s"."buffer"~>"bufferObject"]
        call "glEnableVertexAttribArray" ["i"]
        call "glVertexAttribPointer"
          [ "i", "s"."glSize"
          , ("s"."buffer"~>"glType") `vector_lookup` ("s"."index"), false, 0
          , cast (Const $ Ptr Void) $ ("s"."buffer"~>"offset") `vector_lookup` ("s"."index")
          ]
      else_ $ do
        call "glDisableVertexAttribArray" ["i"]
        switch ("s"."type") $ do
          case_ (nsPat ["Type","FLOAT"]) $ call "glVertexAttrib1f" ["i","s"."_float"]
          case_ (nsPat ["Type","FLOAT_VEC2"]) $ call "glVertexAttrib2fv" ["i", cast (Ptr Float) $ addr $ "s"."_v2f"]
          case_ (nsPat ["Type","FLOAT_VEC3"]) $ call "glVertexAttrib3fv" ["i", cast (Ptr Float) $ addr $ "s"."_v3f"]
          case_ (nsPat ["Type","FLOAT_VEC4"]) $ call "glVertexAttrib4fv" ["i", cast (Ptr Float) $ addr $ "s"."_v4f"]
          case_ (nsPat ["Type","FLOAT_MAT2"]) $ do call "glVertexAttrib2fv" ["i",     cast (Ptr Float) $ addr $ "s"."_m22f"."x"]
                                                   call "glVertexAttrib2fv" ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m22f"."y"]
          case_ (nsPat ["Type","FLOAT_MAT3"]) $ do call "glVertexAttrib3fv" ["i",     cast (Ptr Float) $ addr $ "s"."_m33f"."x"]
                                                   call "glVertexAttrib3fv" ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m33f"."y"]
                                                   call "glVertexAttrib3fv" ["i" + 2, cast (Ptr Float) $ addr $ "s"."_m33f"."z"]
          case_ (nsPat ["Type","FLOAT_MAT4"]) $ do call "glVertexAttrib4fv" ["i",     cast (Ptr Float) $ addr $ "s"."_m44f"."x"]
                                                   call "glVertexAttrib4fv" ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m44f"."y"]
                                                   call "glVertexAttrib4fv" ["i" + 2, cast (Ptr Float) $ addr $ "s"."_m44f"."z"]
                                                   call "glVertexAttrib4fv" ["i" + 3, cast (Ptr Float) $ addr $ "s"."_m44f"."w"]

  procedure "createTexture" ["tx_" :@ SmartPtr "TextureDescriptor"] "Texture" $ do
    var "Texture" ["t"]
    call "glGenTextures" [1,addr $ "t"."texture"]
    varADT "TextureDescriptor" "tx" "tx_"
    varADT "VV2U" "size" $ "tx"~>"textureSize"
    varAssign Int "width" $ "size"~>"_0"."x"
    varAssign Int "height" $ "size"~>"_0"."y"
    var Int ["internalFormat","dataFormat"]
    switch ("tx"~>"textureType"~>"tag") $ do
      case_ (nsPat ["TextureType","tag","Texture2D"]) $ do
        "t"."target" .= "GL_TEXTURE_2D"
        varADT "Texture2D" "tx2D" $ "tx"~>"textureType"
        "internalFormat" .= callExp "textureDataTypeToGLType" ["tx"~>"textureSemantic","tx2D"~>"_0"]
        "dataFormat" .= callExp "textureDataTypeToGLArityType" ["tx"~>"textureSemantic","tx2D"~>"_0"]
      case_ (nsPat ["TextureType","tag","TextureCube"]) $ do
        "t"."target" .= "GL_TEXTURE_CUBE_MAP"
        varADT "TextureCube" "txCube" $ "tx"~>"textureType"
        "internalFormat" .= callExp "textureDataTypeToGLType" ["tx"~>"textureSemantic","txCube"~>"_0"]
        "dataFormat" .= callExp "textureDataTypeToGLArityType" ["tx"~>"textureSemantic","txCube"~>"_0"]
      default_ $ throw "unsupported texture type"

    call "glBindTexture" ["t"."target", "t"."texture"]
    varAssign Int "dataType" $ expIf ("dataFormat" == "GL_DEPTH_COMPONENT") "GL_UNSIGNED_SHORT" "GL_UNSIGNED_BYTE"
    for (varAssign Int "level" $ "tx"~>"textureBaseLevel") ("level" <= "tx"~>"textureMaxLevel") (incExp "level") $ do
      if_ ("t"."target" == "GL_TEXTURE_2D") $ do
        then_ $ do
          call "glTexImage2D" ["t"."target","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
        else_ $ do
          call "glTexImage2D" ["GL_TEXTURE_CUBE_MAP_POSITIVE_X","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          call "glTexImage2D" ["GL_TEXTURE_CUBE_MAP_NEGATIVE_X","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          call "glTexImage2D" ["GL_TEXTURE_CUBE_MAP_POSITIVE_Y","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          call "glTexImage2D" ["GL_TEXTURE_CUBE_MAP_NEGATIVE_Y","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          call "glTexImage2D" ["GL_TEXTURE_CUBE_MAP_POSITIVE_Z","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          call "glTexImage2D" ["GL_TEXTURE_CUBE_MAP_NEGATIVE_Z","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
      "width" /= 2
      "height" /= 2

    -- setup texture sampling
    varADT "SamplerDescriptor" "s" $ "tx"~>"textureSampler"
    call "glTexParameteri" ["t"."target", "GL_TEXTURE_WRAP_S", callExp "edgeMode" ["s"~>"samplerWrapS"]]
    call "glTexParameteri" ["t"."target", "GL_TEXTURE_WRAP_T", callExp "edgeMode" ["s"~>"samplerWrapT"."data"]]
    call "glTexParameteri" ["t"."target", "GL_TEXTURE_MIN_FILTER", callExp "filterMode" ["s"~>"samplerMinFilter"]]
    call "glTexParameteri" ["t"."target", "GL_TEXTURE_MAG_FILTER", callExp "filterMode" ["s"~>"samplerMagFilter"]]
    return_ "t"

  procedure "createStreamData" ["s_" :@ SmartPtr "StreamData"] (SmartPtr "GLStreamData") $ do
    varADT "StreamData" "s" "s_"
    varConstructor (SmartPtr "GLStreamData") "gls" $ new "GLStreamData" []

    switch ("s"~>"streamPrimitive"~>"tag") $ do
      case_ (nsPat ["FetchPrimitive","tag","Points"])    $ "gls"~>"glMode" .= "GL_POINTS"
      case_ (nsPat ["FetchPrimitive","tag","Lines"])     $ "gls"~>"glMode" .= "GL_LINES"
      case_ (nsPat ["FetchPrimitive","tag","Triangles"]) $ "gls"~>"glMode" .= "GL_TRIANGLES"
    varConstructor (SmartPtr "Buffer") "buffer" $ new "Buffer" []
    map_foreach "i" ("s"~>"streamData") $ do
      switch (it_value "i"~>"tag") $ do
        case_ (nsPat ["ArrayValue","tag","VBoolArray"]) $ do
          varADT "VBoolArray" "a" $ it_value "i"
        case_ (nsPat ["ArrayValue","tag","VIntArray"]) $ do
          varADT "VIntArray" "a" $ it_value "i"
        case_ (nsPat ["ArrayValue","tag","VWordArray"]) $ do
          varADT "VWordArray" "a" $ it_value "i"
        case_ (nsPat ["ArrayValue","tag","VFloatArray"]) $ do
          varADT "VFloatArray" "a" $ it_value "i"
          varAssign ("Type") "type" $ callExp "inputType" ["s"~>"streamType" `map_lookup` key "i"]
          call ("gls"~>"streams"."add") [key "i", "type", "buffer", callExp ("buffer"~>"add") ["a"~>"_0"]]
    call ("buffer"~>"freeze") []
    call ("gls"~>"streams"."validate") []

    "gls"~>"glCount" .= 0
    map_foreach "i" ("gls"~>"streams"."map") $ do
      if_ (it_value "i"~>"isArray") $ do
        then_ $ do
          ("gls"~>"glCount") .= ((it_value "i"~>"buffer"~>"size" `vector_lookup` it_value "i"~>"index") / it_value "i"~>"glSize")
          break_
    return_ "gls"

  procedure "createProgram" ["p_" :@ SmartPtr "Program"] (SmartPtr "GLProgram") $ do
    varADT "Program" "p" "p_"
    -- vertex shader
    varAssign UInt "vs" $ callExp "glCreateShader" ["GL_VERTEX_SHADER"]
    varCharPtrFromString "vsSrc" $ "p"~>"vertexShader"
    call "glShaderSource" ["vs",1,addr "vsSrc",nullptr]
    call "glCompileShader" ["vs"]

    -- fragment shader
    varAssign UInt "fs" $ callExp "glCreateShader" ["GL_FRAGMENT_SHADER"]
    varCharPtrFromString "fsSrc" $ "p"~>"fragmentShader"
    call "glShaderSource" ["fs",1,addr "fsSrc",nullptr]
    call "glCompileShader" ["fs"]

    -- create program
    varAssign UInt "po" $ callExp "glCreateProgram" []
    call "glAttachShader" ["po","vs"]
    call "glAttachShader" ["po","fs"]
    call "glLinkProgram" ["po"]

    varConstructor (SmartPtr "GLProgram") "glp" $ new "GLProgram" []
    "glp"~>"program" .= "po"
    "glp"~>"vertexShader" .= "vs"
    "glp"~>"fragmentShader" .= "fs"

    -- query uniforms
    var Int ["loc"]
    map_foreach "i" ("p"~>"programUniforms") $ do
      "loc" .= callExp "glGetUniformLocation" ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ then_ $ do
        "glp"~>"programUniforms" `map_lookup` (key "i") .= "loc"

    -- query sampler uniforms
    map_foreach "i" ("p"~>"programInTextures") $ do
      "loc" .= callExp "glGetUniformLocation" ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ then_ $ do
        "glp"~>"programInTextures" `map_lookup` (key "i") .= "loc"
    -- query vertex attributes
    map_foreach "i" ("p"~>"programStreams") $ do
      "loc" .= callExp "glGetAttribLocation" ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ then_ $ do
        varADT "Parameter" "param" $ it_value "i"
        "glp"~>"programStreams" `map_lookup` (key "i") .= recordValue [("name","param"~>"name"),("index","loc")]
    return_ "glp"

  procedure "setupRasterContext" ["ctx_" :@ SmartPtr "RasterContext"] Void $ do
    switch ("ctx_"~>"tag") $ do
      case_ (nsPat ["RasterContext","tag","PointCtx"]) $ do
        varADT "PointCtx" "ctx" "ctx_"
        switch ("ctx"~>"_0"~>"tag") $ do
          case_ (nsPat ["PointSize","tag","ProgramPointSize"]) $ return ()
          default_ $ do
            throw "unsupported point size"
      case_ (nsPat ["RasterContext","tag","LineCtx"]) $ do
        varADT "LineCtx" "ctx" "ctx_"
        call "glLineWidth" ["ctx"~>"_0"]
      case_ (nsPat ["RasterContext","tag","TriangleCtx"]) $ do
        varADT "TriangleCtx" "ctx" "ctx_"
        switch ("ctx"~>"_0"~>"tag") $ do
          case_ (nsPat ["CullMode","tag","CullNone"]) $ do
            call "glDisable" ["GL_CULL_FACE"]
          case_ (nsPat ["CullMode","tag","CullFront"]) $ do
            varADT "CullFront" "f" $ "ctx"~>"_0"
            call "glEnable" ["GL_CULL_FACE"]
            call "glCullFace" ["GL_FRONT"]
            call "glFrontFace" [callExp "frontFace" ["f"~>"_0"]]
          case_ (nsPat ["CullMode","tag","CullBack"]) $ do
            varADT "CullBack" "f" $ "ctx"~>"_0"
            call "glEnable" ["GL_CULL_FACE"]
            call "glCullFace" ["GL_BACK"]
            call "glFrontFace" [callExp "frontFace" ["f"~>"_0"]]
        call "glDisable" ["GL_POLYGON_OFFSET_FILL"]
        switch ("ctx"~>"_2"~>"tag") $ do
          case_ (nsPat ["PolygonOffset","tag","NoOffset"]) $ return () -- TODO
          case_ (nsPat ["PolygonOffset","tag","Offset"]) $ do
            varADT "Offset" "o" $ "ctx"~>"_2"
            call "glPolygonOffset" ["o"~>"_0","o"~>"_1"]
            call "glEnable" ["GL_POLYGON_OFFSET_FILL"]

  procedure "setupAccumulationContext" ["ctx_" :@ SmartPtr "AccumulationContext"] Void $ do
    varADT "AccumulationContext" "ctx" "ctx_"
    varAssign Bool "noDepth" true
    varAssign Bool "noStencil" true
    varAssign Bool "noColor" true
    vector_foreach "i" ("ctx"~>"accOperations") $ do
      switch ("i"~>"tag") $ do
        case_ (nsPat ["FragmentOperation","tag","DepthOp"]) $ do
          varADT "DepthOp" "o" "i"
          "noDepth" .= false
          varAssign Int "df" $ callExp "comparisonFunction" ["o"~>"_0"]
          if_ ("df" == "GL_ALWAYS" && "o"~>"_1" == false) $ do
            then_ $ call "glDisable" ["GL_DEPTH_TEST"]
            else_ $ do
              call "glEnable" ["GL_DEPTH_TEST"]
              call "glDepthFunc" ["df"]
              call "glDepthMask" ["o"~>"_1"]

        case_ (nsPat ["FragmentOperation","tag","StencilOp"]) $ do
          varADT "StencilOp" "o" "i"
          "noStencil" .= false

        case_ (nsPat ["FragmentOperation","tag","ColorOp"]) $ do
          varADT "ColorOp" "o" "i"
          "noColor" .= false
          switch ("o"~>"_0"~>"tag") $ do
            case_ (nsPat ["Blending","tag","NoBlending"]) $ do
              call "glDisable" ["GL_BLEND"]
            case_ (nsPat ["Blending","tag","BlendLogicOp"]) $ do
              call "glDisable" ["GL_BLEND"]
            case_ (nsPat ["Blending","tag","Blend"]) $ do
              varADT "Blend" "b" $ "o"~>"_0"
              call "glEnable" ["GL_BLEND"]
              call "glBlendEquationSeparate" [callExp "blendEquation" ["b"~>"colorEqSrc"], callExp "blendEquation" ["b"~>"alphaEqSrc"]]
              call "glBlendColor" ["b"~>"color"."x","b"~>"color"."y","b"~>"color"."z","b"~>"color"."w"]
              call "glBlendFuncSeparate" [ callExp "blendingFactor" ["b"~>"colorFSrc"], callExp "blendingFactor" ["b"~>"colorFDst"]
                                         , callExp "blendingFactor" ["b"~>"alphaFSrc"], callExp "blendingFactor" ["b"~>"alphaFDst"]
                                         ]
          varAssign Bool "maskR" true
          varAssign Bool "maskG" true
          varAssign Bool "maskB" true
          varAssign Bool "maskA" true
          switch ("o"~>"_1"~>"tag") $ do
            case_ (nsPat ["Value","tag","VBool"]) $ do
              varADT "VBool" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"
            case_ (nsPat ["Value","tag","VV2B"]) $ do
              varADT "VV2B" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"."x"
              "maskG" .= "v"~>"_0"."y"
            case_ (nsPat ["Value","tag","VV3B"]) $ do
              varADT "VV3B" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"."x"
              "maskG" .= "v"~>"_0"."y"
              "maskB" .= "v"~>"_0"."z"
            case_ (nsPat ["Value","tag","VV4B"]) $ do
              varADT "VV4B" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"."x"
              "maskG" .= "v"~>"_0"."y"
              "maskB" .= "v"~>"_0"."z"
              "maskA" .= "v"~>"_0"."w"
          call "glColorMask" ["maskR","maskG","maskB","maskA"]
    if_ "noStencil" $ then_ $ call "glDisable" ["GL_STENCIL_TEST"]
    if_ "noDepth" $ then_ $ call "glDisable" ["GL_DEPTH_TEST"]

pipelineMethods = do
  method "createRenderTarget" ["t_" :@ SmartPtr "RenderTarget"] UInt $ do
    varADT "RenderTarget" "t" "t_"
    -- does this target have texture attachments?
    varAssign Int "textureCount" 0
    vector_foreach "i_" ("t"~>"renderTargets") $ do
      varADT "TargetItem" "i" "i_"
      if_ ("i"~>"targetRef"."valid" && "i"~>"targetRef"."data"~>"tag" == ns ["ImageRef","tag","TextureImage"]) $ then_ $ do
        inc "textureCount"
    if_ ("textureCount" == 0) $ then_ $ do
      return_ 0
    -- has textures attachment
    var UInt ["fb"]
    call "glGenFramebuffers" [1,addr "fb"]
    call "glBindFramebuffer" ["GL_FRAMEBUFFER", "fb"]
    var Int ["attachment","textarget","level"]
    var UInt ["texture"]
    vector_foreach "i_" ("t"~>"renderTargets") $ do
      varADT "TargetItem" "i" "i_"
      switch ("i"~>"targetSemantic"~>"tag") $ do
        case_ (nsPat ["ImageSemantic","tag","Color"]) $ do 
          "attachment" .= "GL_COLOR_ATTACHMENT0"
        case_ (nsPat ["ImageSemantic","tag","Depth"]) $ do 
          "attachment" .= "GL_DEPTH_ATTACHMENT"
        case_ (nsPat ["ImageSemantic","tag","Stencil"]) $ do 
          "attachment" .= "GL_STENCIL_ATTACHMENT"
      if_ ("i"~>"targetRef"."valid") $ do
        then_ $ switch ("i"~>"targetRef"."data"~>"tag") $ do
          case_ (nsPat ["ImageRef","tag","TextureImage"]) $ do
            varADT "TextureImage" "ti" $ "i"~>"targetRef"."data"
            "texture" .= ("textures" `vector_lookup` ("ti"~>"_0"))."texture"
            "textarget" .= "GL_TEXTURE_2D" -- TODO
            "level" .= "ti"~>"_1"
          case_ (nsPat ["ImageRef","tag","Framebuffer"]) $ do
            "texture" .= 0
            "textarget" .= "GL_TEXTURE_2D"
            "level" .= 0
        else_ $ do
          "texture" .= 0
          "textarget" .= "GL_TEXTURE_2D"
          "level" .= 0
      call "glFramebufferTexture2D" ["GL_FRAMEBUFFER","attachment","textarget","texture","level"]
    return_ "fb"

  constructor ["ppl_" :@ SmartPtr "Pipeline"] $ do
    "screenTarget" .= 0
    "hasCurrentProgram" .= false
    varADT "Pipeline" "ppl" $ "ppl_"
    "pipeline" .= "ppl"
    -- check backend compatibility
    if_ ("ppl"~>"backend"~>"tag" != ns ["Backend","tag","WebGL1"]) $ then_ $ do
      throw "unsupported backend"
    -- allocate all resources
    --  textures
    vector_foreach "i" ("ppl"~>"textures") $ do
      vector_pushBack "textures" $ callExp "createTexture" ["i"]
    --  targets
    vector_foreach "i" ("ppl"~>"targets") $ do
      vector_pushBack "targets" $ callExp "createRenderTarget" ["i"]
    --  programs
    vector_foreach "i" ("ppl"~>"programs") $ do
      vector_pushBack "programs" $ callExp "createProgram" ["i"]
    --  stream data
    vector_foreach "i" ("ppl"~>"streams") $ do
      vector_pushBack "streamData" $ callExp "createStreamData" ["i"]
    call "glReleaseShaderCompiler" []

  destructor $ do
    -- release resources
    -- textures
    vector_foreach "i" "textures" $ do
      call "glDeleteTextures" [1,addr "i"."texture"]
    -- targets
    vector_foreach "i" "targets" $ do
      call "glDeleteFramebuffers" [1,addr "i"]
    -- programs
    vector_foreach "i" "programs" $ do
      call "glDeleteProgram" ["i"~>"program"]
      call "glDeleteShader" ["i"~>"vertexShader"]
      call "glDeleteShader" ["i"~>"fragmentShader"]

  method "setPipelineInput" ["i" :@ SmartPtr "PipelineInput"] Void $ do
    "input" .= "i"

  method "render" [] Void $ do
    vector_foreach "i" ("pipeline"~>"commands") $ do
      switch ("i"~>"tag") $ do
        case_ (nsPat ["Command","tag","SetRasterContext"]) $ do
          varADT "SetRasterContext" "cmd" $ "i"
          call "setupRasterContext" ["cmd"~>"_0"]
        case_ (nsPat ["Command","tag","SetAccumulationContext"]) $ do
          varADT "SetAccumulationContext" "cmd" $ "i"
          call "setupAccumulationContext" ["cmd"~>"_0"]
        case_ (nsPat ["Command","tag","SetTexture"]) $ do
          varADT "SetTexture" "cmd" $ "i"
          call "glActiveTexture" ["GL_TEXTURE0" + "cmd"~>"_0"]
          call "glBindTexture" [("textures" `vector_lookup` ("cmd"~>"_1"))."target",("textures" `vector_lookup` ("cmd"~>"_1"))."texture"]
        case_ (nsPat ["Command","tag","SetProgram"]) $ do
          varADT "SetProgram" "cmd" $ "i"
          "hasCurrentProgram" .= true
          "currentProgram" .= "cmd"~>"_0"
          call "glUseProgram" [("programs" `vector_lookup` "currentProgram")~>"program"]
        case_ (nsPat ["Command","tag","SetRenderTarget"]) $ do
          varADT "SetRenderTarget" "cmd" $ "i"
          varAssign UInt "t" $ "targets" `vector_lookup` ("cmd"~>"_0")
          call "glBindFramebuffer" ["GL_FRAMEBUFFER", expIf ("t"==0) "screenTarget" "t"]
          if_ (notNull "input") $ do
            then_ $ call "glViewport" [0,0,"input"~>"screenWidth","input"~>"screenHeight"]
        case_ (nsPat ["Command","tag","ClearRenderTarget"]) $ do
          varADT "ClearRenderTarget" "cmd" $ "i"
          varAssign UInt "mask" 0
          vector_foreach "a" ("cmd"~>"_0") $ do
            varADT "ClearImage" "image" $ "a"
            switch ("image"~>"imageSemantic"~>"tag") $ do
              case_ (nsPat ["ImageSemantic","tag","Depth"]) $ do
                varADT "VFloat" "v" $ "image"~>"clearValue"
                call "glDepthMask" [true]
                call "glClearDepthf" ["v"~>"_0"]
                "mask" |= "GL_DEPTH_BUFFER_BIT"
              case_ (nsPat ["ImageSemantic","tag","Stencil"]) $ do
                varADT "VWord" "v" $ "image"~>"clearValue"
                call "glClearStencil" ["v"~>"_0"]
                "mask" |= "GL_STENCIL_BUFFER_BIT"
              case_ (nsPat ["ImageSemantic","tag","Color"]) $ do
                switch ("image"~>"clearValue"~>"tag") $ do
                  case_ (nsPat ["Value","tag","VFloat"]) $ do
                    varADT "VFloat" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0",0.0,0.0,1.0]
                  case_ (nsPat ["Value","tag","VV2F"]) $ do
                    varADT "VV2F" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0"."x","v"~>"_0"."y",0.0,1.0]
                  case_ (nsPat ["Value","tag","VV3F"]) $ do
                    varADT "VV3F" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0"."x","v"~>"_0"."y","v"~>"_0"."z",1.0]
                  case_ (nsPat ["Value","tag","VV4F"]) $ do
                    varADT "VV4F" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0"."x","v"~>"_0"."y","v"~>"_0"."z","v"~>"_0"."w"]
                  default_ $ do
                    call "glClearColor" [0.0,0.0,0.0,1.0]
                call "glColorMask" [true,true,true,true]
                "mask" |= "GL_COLOR_BUFFER_BIT"
          call "glClear" ["mask"]
        case_ (nsPat ["Command","tag","SetSamplerUniform"]) $  if_ "hasCurrentProgram" $ then_ $ do
          varADT "SetSamplerUniform" "cmd" $ "i"
          varAssign Int "sampler" $ ("programs" `vector_lookup` "currentProgram")~>"programInTextures" `map_lookup` ("cmd"~>"_0")
          call "glUniform1i" ["sampler","cmd"~>"_1"]
        case_ (nsPat ["Command","tag","RenderSlot"]) $ if_ (notNull "input" && notNull "pipeline" && "hasCurrentProgram") $ then_ $ do
          varADT "RenderSlot" "cmd" $ "i"
          varADT "Slot" "slot" $ "pipeline"~>"slots" `vector_lookup` ("cmd"~>"_0")
          if_ (map_notElem ("input"~>"objectMap") ("slot"~>"slotName")) $ then_ break_
          map_foreach "o" (deref $ "input"~>"objectMap" `map_lookup` ("slot"~>"slotName")) $ do
            if_ (not $ "o"~>"enabled") $ then_ continue_
            -- setup uniforms
            map_foreach "u" (("programs" `vector_lookup` "currentProgram")~>"programUniforms") $ do
              if_ (map_elem ("o"~>"uniforms") (key "u")) $ do
                then_ $ call "setUniformValue" [it_value "u","o"~>"uniforms" `map_lookup` (key "u")]
                else_ $ call "setUniformValue" [it_value "u","input"~>"uniforms" `map_lookup` (key "u")]
            -- setup streams
            map_foreach "s" (("programs" `vector_lookup` "currentProgram")~>"programStreams") $ do
              call "setStream" [it_value "s"."index",deref $ "o"~>"streams"~>"map" `map_lookup` (it_value "s"."name")]
            -- draw call
            -- TODO: support index buffers
            call "glDrawArrays" ["o"~>"glMode", 0, "o"~>"glCount"]
        case_ (nsPat ["Command","tag","RenderStream"]) $ if_ (notNull "input" && notNull "pipeline" && "hasCurrentProgram") $ then_ $ do
          varADT "RenderStream" "cmd" $ "i"
          varAssign (SmartPtr "GLStreamData") "data" $ "streamData" `vector_lookup` ("cmd"~>"_0")
          -- setup streams
          map_foreach "s" (("programs" `vector_lookup` "currentProgram")~>"programStreams") $ do
            call "setStream" [it_value "s"."index",deref $ "data"~>"streams"."map" `map_lookup` (it_value "s"."name")]
          -- draw call
          -- TODO: support index buffers
          call "glDrawArrays" ["data"~>"glMode", 0, "data"~>"glCount"]

hpp = do
  class_ "Buffer" $ do
    public $ do
      memberVar (Vector Int) ["size","byteSize","glType"]
      memberVar (Vector Long) ["offset"]
      memberVar (Vector (Ptr Void)) ["data"]
      memberVar UInt ["bufferObject"]

      method "add" ["v" :@ Ref (Vector Int8)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" $ vector_dataPtr "v"
        vector_pushBack "size" $ vector_size "v"
        vector_pushBack "byteSize" $ vector_size "v"
        vector_pushBack "glType" "GL_BYTE"
        return_ "i"

      method "add" ["v" :@ Ref (Vector UInt8)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" $ vector_dataPtr "v"
        vector_pushBack "size" $ vector_size "v"
        vector_pushBack "byteSize" $ vector_size "v"
        vector_pushBack "glType" "GL_UNSIGNED_BYTE"
        return_ "i"

      method "add" ["v" :@ Ref (Vector Int16)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" $ vector_dataPtr "v"
        vector_pushBack "size" $ vector_size "v"
        vector_pushBack "byteSize" $ 2 * vector_size "v"
        vector_pushBack "glType" "GL_SHORT"
        return_ "i"

      method "add" ["v" :@ Ref (Vector UInt16)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" $ vector_dataPtr "v"
        vector_pushBack "size" $ vector_size "v"
        vector_pushBack "byteSize" $ 2 * vector_size "v"
        vector_pushBack "glType" "GL_UNSIGNED_SHORT"
        return_ "i"

      method "add" ["v" :@ Ref (Vector Float)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" $ vector_dataPtr "v"
        vector_pushBack "size" $ vector_size "v"
        vector_pushBack "byteSize" $ 4 * vector_size "v"
        vector_pushBack "glType" "GL_FLOAT"
        return_ "i"

      method "freeze" [] Void $ do
        varAssign UInt "bufferSize" 0
        vector_foreach "i" "byteSize" $ do
          vector_pushBack "offset" "bufferSize"
          "bufferSize" += "i"

        var UInt ["bo"]
        call "glGenBuffers" [1,addr "bo"]
        call "glBindBuffer" ["GL_ARRAY_BUFFER","bo"]
        call "glBufferData" ["GL_ARRAY_BUFFER","bufferSize",nullptr,"GL_STATIC_DRAW"]
        varAssign UInt "offset_" 0
        for_range "i" 0 (vector_size "data") $ do
          call "glBufferSubData" ["GL_ARRAY_BUFFER","offset_","byteSize" `vector_lookup` "i","data" `vector_lookup` "i"]
          "offset_" += ("byteSize" `vector_lookup` "i")
        "bufferObject" .= "bo"

      method "update" ["i" :@ Int, "v" :@ Ref (Vector Float)] Void $ return () -- TODO

  enum_ "Type"
    [ "FLOAT"
    , "FLOAT_VEC2"
    , "FLOAT_VEC3"
    , "FLOAT_VEC4"
    , "FLOAT_MAT2"
    , "FLOAT_MAT3"
    , "FLOAT_MAT4"
    ]

  class_ "Stream" $ do
    public $ do
      memberVar "Type" ["type"]
      memberVar (SmartPtr "Buffer") ["buffer"]
      memberVar Int ["index"]
      memberVar Bool ["isArray"]
      memberVar Int ["glSize"]
      memberUnion
        [ "_float"  :@ Float
        , "_v2f"    :@ "V2F"
        , "_v3f"    :@ "V3F"
        , "_v4f"    :@ "V4F"
        , "_m22f"   :@ "M22F"
        , "_m33f"   :@ "M33F"
        , "_m44f"   :@ "M44F"
        ]

      constructor ["v" :@ Ref Float] $ do
        "type"    .= ns ["Type","FLOAT"]
        "isArray" .= false
        "_float"  .= "v"
        "glSize"  .= 1
      constructor ["v" :@ Ref "V2F"] $ do
        "type"    .= ns ["Type","FLOAT_VEC2"]
        "isArray" .= false
        "_v2f"    .= "v"
        "glSize"  .= 2
      constructor ["v" :@ Ref "V3F"] $ do
        "type"    .= ns ["Type","FLOAT_VEC3"]
        "isArray" .= false
        "_v3f"    .= "v"
        "glSize"  .= 3
      constructor ["v" :@ Ref "V4F"] $ do
        "type"    .= ns ["Type","FLOAT_VEC4"]
        "isArray" .= false
        "_v4f"    .= "v"
        "glSize"  .= 4
      constructor ["v" :@ Ref "M22F"] $ do
        "type"    .= ns ["Type","FLOAT_MAT2"]
        "isArray" .= false
        "_m22f"    .= "v"
        "glSize"  .= 4
      constructor ["v" :@ Ref "M33F"] $ do
        "type"    .= ns ["Type","FLOAT_MAT3"]
        "isArray" .= false
        "_m33f"   .= "v"
        "glSize"  .= 9
      constructor ["v" :@ Ref "M44F"] $ do
        "type"    .= ns ["Type","FLOAT_MAT4"]
        "isArray" .= false
        "_m44f"   .= "v"
        "glSize"  .= 16
      constructor ["b" :@ SmartPtr "Buffer", "index" :@ Int, "t" :@ "Type"] $ do
        "type"    .= "t"
        "buffer"  .= "b"
        "index"   .= "i"
        "isArray" .= true
        "glSize"  .= 16
        switch "t" $ do
          case_ (nsPat ["Type","FLOAT"]) $      "glSize"  .= 1
          case_ (nsPat ["Type","FLOAT_VEC2"]) $ "glSize"  .= 2
          case_ (nsPat ["Type","FLOAT_VEC3"]) $ "glSize"  .= 3
          case_ (nsPat ["Type","FLOAT_VEC4"]) $ "glSize"  .= 4
          case_ (nsPat ["Type","FLOAT_MAT2"]) $ "glSize"  .= 4
          case_ (nsPat ["Type","FLOAT_MAT3"]) $ "glSize"  .= 9
          case_ (nsPat ["Type","FLOAT_MAT4"]) $ "glSize"  .= 16

  class_ "StreamMap" $ do
    public $ do
      memberVar (Map String (SmartPtr "Stream")) ["map"]

      -- TODO: Map_insert needed
      method "add" ["name" :@ String, "v" :@ Ref Float] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "V2F"] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "V3F"] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "V4F"] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "M22F"] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "M33F"] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "M44F"] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["v"]
      method "add" ["name" :@ String, "t" :@ "Type", "b" :@ SmartPtr "Buffer", "index" :@ Int] Void $ map_insert "map" "name" $ new_SmartPtr $ new "Stream" ["b","index","t"]
      method "validate" [] Bool $ return_ true -- TODO

  enum_ "Primitive"
    [ "TriangleStrip"
    , "TriangleList"
    , "TriangleFan"
    , "LineStrip"
    , "LineList"
    , "LineLoop"
    , "PointList"
    ]

  struct_ "UniformValue" $ do
    memberVar (Enum "InputType::tag") ["tag"] -- TODO
    memberUnion
      [ "_int"   :@ Int
      , "_word"  :@ UInt
      , "_float" :@ Float
      , "_bool"  :@ Bool
      , "_v2i"   :@ "V2I"
      , "_v2u"   :@ "V2U"
      , "_v2b"   :@ "V2B"
      , "_v2f"   :@ "V2F"
      , "_v3i"   :@ "V3I"
      , "_v3u"   :@ "V3U"
      , "_v3b"   :@ "V3B"
      , "_v3f"   :@ "V3F"
      , "_v4i"   :@ "V4I"
      , "_v4u"   :@ "V4U"
      , "_v4b"   :@ "V4B"
      , "_v4f"   :@ "V4F"
      , "_m22f"  :@ "M22F"
      , "_m33f"  :@ "M33F"
      , "_m44f"  :@ "M44F"
      ]

  class_ "Object" $ do
    public $ do
      memberVar Bool ["enabled"]
      memberVar Int ["order","glMode","glCount"]
      memberVar (Map String "UniformValue") ["uniforms"]
      memberVar (SmartPtr "StreamMap") ["streams"]

      destructor $ return () -- TODO

      method "enable" ["visible" :@ Bool] Void $ "enabled" .= "visible"
      method "setOrder" ["order" :@ Int] Void $ "order" .= "o"

      method "setUniform" ["name" :@ String, "v" :@ Ref Int] Void $   map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Int"]),   ("_int","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref UInt] Void $  map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Word"]),  ("_word","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref Float] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Float"]), ("_float","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref Bool] Void $  map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Bool"]),  ("_bool","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2I"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2I"]),   ("_v2i","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2U"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2U"]),   ("_v2u","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2F"]),   ("_v2f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2B"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2B"]),   ("_v2b","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3I"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3I"]),   ("_v3i","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3U"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3U"]),   ("_v3u","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3F"]),   ("_v3f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3B"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3B"]),   ("_v3b","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4I"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4I"]),   ("_v4i","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4U"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4U"]),   ("_v4u","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4F"]),   ("_v4f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4B"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4B"]),   ("_v4b","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "M22F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","M22F"]), ("_m22f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "M33F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","M33F"]), ("_m33f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "M44F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","M44F"]), ("_m44f","v")]

  class_ "PipelineInput" $ do
    public $ do
      memberVar (Map String (SmartPtr (Vector (SmartPtr "Object")))) ["objectMap"]
      memberVar (Map String "UniformValue") ["uniforms"]
      memberVar Int ["screenWidth","screenHeight"]

      method "createObject" ["slotName" :@ String, "prim" :@ "Primitive", "attributes" :@ SmartPtr "StreamMap", "objectUniforms" :@ Vector String] (SmartPtr "Object") $ do
        -- std::shared_ptr<Object> o(new Object());
        "o"~>"enabled" .= true
        "o"~>"order" .= 0
        "o"~>"glMode" .= callExp "primitiveMode" ["prim"]
        varAssign Int "count" 0
        map_foreach "i" ("attributes"~>"map") $ do
          if_ (it_value "i" ~> "isArray") $ then_ $ do
            "count" .= it_value "i" ~> "buffer" ~> ("size" `vector_lookup` (it_value "i" ~> "index")) / it_value "i" ~> "glSize"
            break_
        "o"~>"glCount" .= "count"
        "o"~>"streams" .= "attributes"
        if_ (map_elem "objectMap" "slotName") $ do
          then_ $ do
            map_insert "objectMap" "slotName" "o"
          else_ $ do
            --map_insert "objectMap" "slotName" $ 
            --objectMap[slotName] = std::shared_ptr<std::vector<std::shared_ptr<Object>>>(new std::vector<std::shared_ptr<Object>>({o}));
            return () -- TODO
        return_ "o"

      method "createObject" ["slotName" :@ String, "prim" :@ "Primitive", "attributes" :@ Ref "StreamMap"
                            , "indexBuffer" :@ Ref "Buffer", "bufferIndex" :@ Int, "objectUniforms" :@ Vector String] (SmartPtr "Object") $ return () -- TODO
      method "sortSlotObjects" [] Void $ return () -- TODO
      method "setScreenSize" ["w" :@ Int, "h" :@ Int] Void $ do
        "screenWidth" .= "w"
        "screenHeight" .= "h"

      method "setUniform" ["name" :@ String, "v" :@ Ref Int] Void $   map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Int"]),   ("_int","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref UInt] Void $  map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Word"]),  ("_word","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref Float] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Float"]), ("_float","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref Bool] Void $  map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","Bool"]),  ("_bool","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2I"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2I"]),   ("_v2i","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2U"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2U"]),   ("_v2u","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2F"]),   ("_v2f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V2B"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V2B"]),   ("_v2b","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3I"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3I"]),   ("_v3i","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3U"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3U"]),   ("_v3u","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3F"]),   ("_v3f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V3B"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V3B"]),   ("_v3b","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4I"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4I"]),   ("_v4i","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4U"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4U"]),   ("_v4u","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4F"]),   ("_v4f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "V4B"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","V4B"]),   ("_v4b","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "M22F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","M22F"]), ("_m22f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "M33F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","M33F"]), ("_m33f","v")]
      method "setUniform" ["name" :@ String, "v" :@ Ref "M44F"] Void $ map_insert "uniforms" "name" $ recordValue [("tag",ns ["InputType","tag","M44F"]), ("_m44f","v")]

  struct_ "Texture" $ do
    memberVar Int ["target"]
    memberVar UInt ["texture"]

  struct_ "StreamInfo" $ do
    memberVar String ["name"]
    memberVar Int ["index"]

  class_ "GLProgram" $ do
    public $ do
      memberVar UInt ["program","vertexShader","fragmentShader"]
      memberVar (Map String Int) ["programUniforms","programInTextures"]
      memberVar (Map String "StreamInfo") ["programStreams"]

  struct_ "GLStreamData" $ do
    memberVar Int ["glMode","glCount"]
    memberVar "StreamMap" ["streams"]

  class_ "GLES20Pipeline" $ do
    public $ do
      constructor ["ppl" :@ SmartPtr "Pipeline"] $ return () -- TODO
      destructor $ return () -- TODO

      method "setPipelineInput" ["i" :@ SmartPtr "PipelineInput"] Void $ return () -- TODO
      method "render" [] Void $ return () -- TODO

      memberVar UInt ["screenTarget"]

    private $ do
      method "createRenderTarget" ["t_" :@ SmartPtr "RenderTarget"] UInt $ return () -- TODO

      memberVar (SmartPtr "PipelineInput") ["input"]
      memberVar (SmartPtr "data::Pipeline") ["pipeline"] -- TODO: type namespace
      memberVar (Vector "Texture") ["textures"]
      memberVar UInt ["targets"]
      memberVar (Vector (SmartPtr "GLProgram")) ["programs"]
      memberVar (Vector (SmartPtr "GLStreamData")) ["streamData"]
      memberVar UInt ["currentProgram"]
      memberVar Bool ["hasCurrentProgram"]

backend = do
  enumConversions
  globalFunctions
  pipelineMethods
