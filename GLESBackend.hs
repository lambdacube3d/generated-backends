{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Prelude (($),Num (..))
import Language

enumConversions = do
  procedure "inputType" ["t" :. SmartPtr "InputType"] (Enum "Type") $ do
    switch ("t"~>"tag") $ do
      case_ (ns ["InputType","tag","Float"]) $ return_ $ ns ["Type","FLOAT"]
      case_ (ns ["InputType","tag","V2F"])   $ return_ $ ns ["Type","FLOAT_VEC2"]
      case_ (ns ["InputType","tag","V3F"])   $ return_ $ ns ["Type","FLOAT_VEC3"]
      case_ (ns ["InputType","tag","V4F"])   $ return_ $ ns ["Type","FLOAT_VEC4"]
      case_ (ns ["InputType","tag","M22F"])  $ return_ $ ns ["Type","FLOAT_MAT2"]
      case_ (ns ["InputType","tag","M33F"])  $ return_ $ ns ["Type","FLOAT_MAT3"]
      case_ (ns ["InputType","tag","M44F"])  $ return_ $ ns ["Type","FLOAT_MAT4"]
    throw "illegal input type"

  procedure "primitiveMode" ["p" :. "Primitive"] Int $ do
    switch "p" $ do
      case_ (ns ["Primitive","TriangleStrip"]) $ return_ "GL_TRIANGLE_STRIP"
      case_ (ns ["Primitive","TriangleList"])  $ return_ "GL_TRIANGLES"
      case_ (ns ["Primitive","TriangleFan"])   $ return_ "GL_TRIANGLE_FAN"
      case_ (ns ["Primitive","LineStrip"])     $ return_ "GL_LINE_STRIP"
      case_ (ns ["Primitive","LineList"])      $ return_ "GL_LINES"
      case_ (ns ["Primitive","LineLoop"])      $ return_ "GL_LINE_LOOP"
      case_ (ns ["Primitive","PointList"])     $ return_ "GL_POINTS"

  procedure "blendingFactor" ["bf" :. SmartPtr "BlendingFactor"] Int $ do
    switch ("bf"~>"tag") $ do
      case_ (ns ["BlendingFactor","tag","ConstantAlpha"]) $ return_ "GL_CONSTANT_ALPHA"
      case_ (ns ["BlendingFactor","tag","ConstantColor"]) $ return_ "GL_CONSTANT_COLOR"
      case_ (ns ["BlendingFactor","tag","DstAlpha"]) $ return_ "GL_DST_ALPHA"
      case_ (ns ["BlendingFactor","tag","DstColor"]) $ return_ "GL_DST_COLOR"
      case_ (ns ["BlendingFactor","tag","One"]) $ return_ "GL_ONE"
      case_ (ns ["BlendingFactor","tag","OneMinusConstantAlpha"]) $ return_ "GL_ONE_MINUS_CONSTANT_ALPHA"
      case_ (ns ["BlendingFactor","tag","OneMinusConstantColor"]) $ return_ "GL_ONE_MINUS_CONSTANT_COLOR"
      case_ (ns ["BlendingFactor","tag","OneMinusDstAlpha"]) $ return_ "GL_ONE_MINUS_DST_ALPHA"
      case_ (ns ["BlendingFactor","tag","OneMinusDstColor"]) $ return_ "GL_ONE_MINUS_DST_COLOR"
      case_ (ns ["BlendingFactor","tag","OneMinusSrcAlpha"]) $ return_ "GL_ONE_MINUS_SRC_ALPHA"
      case_ (ns ["BlendingFactor","tag","OneMinusSrcColor"]) $ return_ "GL_ONE_MINUS_SRC_COLOR"
      case_ (ns ["BlendingFactor","tag","SrcAlpha"]) $ return_ "GL_SRC_ALPHA"
      case_ (ns ["BlendingFactor","tag","SrcAlphaSaturate"]) $ return_ "GL_SRC_ALPHA_SATURATE"
      case_ (ns ["BlendingFactor","tag","SrcColor"]) $ return_ "GL_SRC_COLOR"
      case_ (ns ["BlendingFactor","tag","Zero"]) $ return_ "GL_ZERO"
    throw "illegal blending factor"

  procedure "blendEquation" ["be" :. SmartPtr "BlendEquation"] Int $ do
    switch ("be"~>"tag") $ do
      case_ (ns ["BlendEquation","tag","FuncAdd"]) $ return_ "GL_FUNC_ADD"
      case_ (ns ["BlendEquation","tag","FuncReverseSubtract"]) $ return_ "GL_FUNC_REVERSE_SUBTRACT"
      case_ (ns ["BlendEquation","tag","FuncSubtract"]) $ return_ "GL_FUNC_SUBTRACT"
    throw "illegal blend equation"

  procedure "comparisonFunction" ["cf" :. SmartPtr "ComparisonFunction"] Int $ do
    switch ("ce"~>"tag") $ do
      case_ (ns ["ComparisonFunction","tag","Always"]) $ return_ "GL_ALWAYS"
      case_ (ns ["ComparisonFunction","tag","Equal"]) $ return_ "GL_EQUAL"
      case_ (ns ["ComparisonFunction","tag","Gequal"]) $ return_ "GL_GEQUAL"
      case_ (ns ["ComparisonFunction","tag","Greater"]) $ return_ "GL_GREATER"
      case_ (ns ["ComparisonFunction","tag","Lequal"]) $ return_ "GL_LEQUAL"
      case_ (ns ["ComparisonFunction","tag","Less"]) $ return_ "GL_LESS"
      case_ (ns ["ComparisonFunction","tag","Never"]) $ return_ "GL_NEVER"
      case_ (ns ["ComparisonFunction","tag","Notequal"]) $ return_ "GL_NOTEQUAL"
    throw "illegal comparison function"

  procedure "frontFace" ["ff" :. SmartPtr "FrontFace"] Int $ do
    switch ("ff"~>"tag") $ do
      case_ (ns ["FrontFace","tag","CCW"]) $ return_ "GL_CCW"
      case_ (ns ["FrontFace","tag","CW"]) $ return_ "GL_CW"
    throw "illegal front face value"

  procedure "textureDataTypeToGLType" ["s_" :. SmartPtr "ImageSemantic", "d_" :. SmartPtr "TextureDataType"] Int $ do
    switch ("s_"~>"tag") $ do
      case_ (ns ["ImageSemantic","tag","Color"]) $ return_ "GL_RGBA"
      case_ (ns ["ImageSemantic","tag","Depth"]) $ return_ "GL_DEPTH_COMPONENT"
    throw "FIXME: This texture format is not yet supported"

  procedure "textureDataTypeToGLArityType" ["s_" :. SmartPtr "ImageSemantic", "d_" :. SmartPtr "TextureDataType"] Int $ do
    switch ("s_"~>"tag") $ do
      case_ (ns ["ImageSemantic","tag","Color"]) $ return_ "GL_RGBA"
      case_ (ns ["ImageSemantic","tag","Depth"]) $ return_ "GL_DEPTH_COMPONENT"
    throw "FIXME: This texture format is not yet supported"

  procedure "edgeMode" ["e" :. SmartPtr "EdgeMode"] Int $ do
    switch ("e"~>"tag") $ do
      case_ (ns ["EdgeMode","tag","ClampToEdge"]) $ return_ "GL_CLAMP_TO_EDGE"
      case_ (ns ["EdgeMode","tag","Repeat"]) $ return_ "GL_REPEAT"
      case_ (ns ["EdgeMode","tag","MirroredRepeat"]) $ return_ "GL_MIRRORED_REPEAT"
      default_ $ throw "unsupported edge mode"

  procedure "filterMode" ["f" :. SmartPtr "Filter"] Int $ do
    switch ("f"~>"tag") $ do
      case_ (ns ["Filter","tag","Nearest"]) $ return_ "GL_NEAREST"
      case_ (ns ["Filter","tag","Linear"]) $ return_ "GL_LINEAR"
      case_ (ns ["Filter","tag","NearestMipmapNearest"]) $ return_ "GL_NEAREST_MIPMAP_NEAREST"
      case_ (ns ["Filter","tag","NearestMipmapLinear"]) $ return_ "GL_NEAREST_MIPMAP_LINEAR"
      case_ (ns ["Filter","tag","LinearMipmapNearest"]) $ return_ "GL_LINEAR_MIPMAP_NEAREST"
      case_ (ns ["Filter","tag","LinearMipmapLinear"]) $ return_ "GL_LINEAR_MIPMAP_LINEAR"
      default_ $ throw "unsupported filter mode"

globalFunctions = do
  procedure "setUniformValue" ["i" :. Int, "v" :. Ref "UniformValue"] Void $ do
    switch ("v"."tag") $ do
      case_ (ns ["InputType","tag","Int"])    $ call "glUniform1i" ["i","v"."_int"]
      case_ (ns ["InputType","tag","Word"])   $ call "glUniform1i" ["i","v"."_word"]
      case_ (ns ["InputType","tag","Float"])  $ call "glUniform1f" ["i","v"."_float"]
      case_ (ns ["InputType","tag","Bool"])   $ call "glUniform1i" ["i","v"."_bool"]
      case_ (ns ["InputType","tag","V2I"])    $ call "glUniform2iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v2i"]
      case_ (ns ["InputType","tag","V2U"])    $ call "glUniform2iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v2u"]
      case_ (ns ["InputType","tag","V2F"])    $ call "glUniform2fv" ["i",1,cast (Ptr Float) $ addr $ "v"."_v2f"]
      case_ (ns ["InputType","tag","V2B"])    $ call "glUniform2iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v2b"]

      case_ (ns ["InputType","tag","V3I"])    $ call "glUniform3iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v3i"]
      case_ (ns ["InputType","tag","V3U"])    $ call "glUniform3iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v3u"]
      case_ (ns ["InputType","tag","V3F"])    $ call "glUniform3fv" ["i",1,cast (Ptr Float) $ addr $ "v"."_v3f"]
      case_ (ns ["InputType","tag","V3B"])    $ call "glUniform3iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v3b"]

      case_ (ns ["InputType","tag","V4I"])    $ call "glUniform4iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v4i"]
      case_ (ns ["InputType","tag","V4U"])    $ call "glUniform4iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v4u"]
      case_ (ns ["InputType","tag","V4F"])    $ call "glUniform4fv" ["i",1,cast (Ptr Float) $ addr $ "v"."_v4f"]
      case_ (ns ["InputType","tag","V4B"])    $ call "glUniform4iv" ["i",1,cast (Ptr Int) $ addr $ "v"."_v4b"]

      case_ (ns ["InputType","tag","M22F"])   $ call "glUniformMatrix2fv" ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m22f"]
      case_ (ns ["InputType","tag","M33F"])   $ call "glUniformMatrix3fv" ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m33f"]
      case_ (ns ["InputType","tag","M44F"])   $ call "glUniformMatrix4fv" ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m44f"]

  procedure "setStream" ["i" :. Int, "s" :. Ref "Stream"] Void $ do
    if_ ("s"."isArray") $ do
      then_ $ do
        call "glBindBuffer" ["GL_ARRAY_BUFFER","s"."buffer"~>"bufferObject"]
        call "glEnableVertexAttribArray" ["i"]
        call "glVertexAttribPointer"
          [ "i", "s"."glSize"
          , ("s"."buffer"~>"glType") `vectorLookup` ("s"."index"), false, 0
          , cast (Const $ Ptr Void) $ ("s"."buffer"~>"offset") `vectorLookup` ("s"."index")
          ]
      else_ $ do
        call "glDisableVertexAttribArray" ["i"]
        switch ("s"."type") $ do
          case_ (ns ["Type","FLOAT"]) $ call "glVertexAttrib1f" ["i","s"."_float"]
          case_ (ns ["Type","FLOAT_VEC2"]) $ call "glVertexAttrib2fv" ["i", cast (Ptr Float) $ addr $ "s"."_v2f"]
          case_ (ns ["Type","FLOAT_VEC3"]) $ call "glVertexAttrib3fv" ["i", cast (Ptr Float) $ addr $ "s"."_v3f"]
          case_ (ns ["Type","FLOAT_VEC4"]) $ call "glVertexAttrib4fv" ["i", cast (Ptr Float) $ addr $ "s"."_v4f"]
          case_ (ns ["Type","FLOAT_MAT2"]) $ do call "glVertexAttrib2fv" ["i",     cast (Ptr Float) $ addr $ "s"."_m22f"."x"]
                                                call "glVertexAttrib2fv" ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m22f"."y"]
          case_ (ns ["Type","FLOAT_MAT3"]) $ do call "glVertexAttrib3fv" ["i",     cast (Ptr Float) $ addr $ "s"."_m33f"."x"]
                                                call "glVertexAttrib3fv" ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m33f"."y"]
                                                call "glVertexAttrib3fv" ["i" + 2, cast (Ptr Float) $ addr $ "s"."_m33f"."z"]
          case_ (ns ["Type","FLOAT_MAT4"]) $ do call "glVertexAttrib4fv" ["i",     cast (Ptr Float) $ addr $ "s"."_m44f"."x"]
                                                call "glVertexAttrib4fv" ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m44f"."y"]
                                                call "glVertexAttrib4fv" ["i" + 2, cast (Ptr Float) $ addr $ "s"."_m44f"."z"]
                                                call "glVertexAttrib4fv" ["i" + 3, cast (Ptr Float) $ addr $ "s"."_m44f"."w"]

  procedure "createTexture" ["tx_" :. SmartPtr "TextureDescriptor"] "Texture" $ do
    var "Texture" ["t"]
    call "glGenTextures" [1,addr $ "t"."texture"]
    varADT "TextureDescriptor" "tx" "tx_"
    varADT "VV2U" "size" $ "tx"~>"textureSize"
    varAssign Int "width" $ "size"~>"_0"."x"
    varAssign Int "height" $ "size"~>"_0"."y"
    var Int ["internalFormat","dataFormat"]
    switch ("tx"~>"textureType"~>"tag") $ do
      case_ (ns ["TextureType","tag","Texture2D"]) $ do
        "t"."target" .= "GL_TEXTURE_2D"
        varADT "Texture2D" "tx2D" $ "tx"~>"textureType"
        "internalFormat" .= callExp "textureDataTypeToGLType" ["tx"~>"textureSemantic","tx2D"~>"_0"]
        "dataFormat" .= callExp "textureDataTypeToGLArityType" ["tx"~>"textureSemantic","tx2D"~>"_0"]
      case_ (ns ["TextureType","tag","TextureCube"]) $ do
        "t"."target" .= "GL_TEXTURE_CUBE_MAP"
        varADT "TextureCube" "txCube" $ "tx"~>"textureType"
        "internalFormat" .= callExp "textureDataTypeToGLType" ["tx"~>"textureSemantic","txCube"~>"_0"]
        "dataFormat" .= callExp "textureDataTypeToGLArityType" ["tx"~>"textureSemantic","txCube"~>"_0"]
      default_ $ throw "unsupported texture type"

    call "glBindTexture" ["t"."target", "t"."texture"]
    varAssign Int "dataType" $ expIf ("dataFormat" == "GL_DEPTH_COMPONENT") "GL_UNSIGNED_SHORT" "GL_UNSIGNED_BYTE"
    for (varAssign Int "level" $ "tx"~>"textureBaseLevel") ("level" <= "tx"~>"textureMaxLevel") (inc "level") $ do
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

  procedure "createStreamData" ["s_" :. SmartPtr "StreamData"] (SmartPtr "GLStreamData") $ do
    varADT "StreamData" "s" "s_"
    varConstructor (SmartPtr "GLStreamData") "gls" $ new "GLStreamData" []

    switch ("s"~>"streamPrimitive"~>"tag") $ do
      case_ (ns ["FetchPrimitive","tag","Points"])    $ "gls"~>"glMode" .= "GL_POINTS"
      case_ (ns ["FetchPrimitive","tag","Lines"])     $ "gls"~>"glMode" .= "GL_LINES"
      case_ (ns ["FetchPrimitive","tag","Triangles"]) $ "gls"~>"glMode" .= "GL_TRIANGLES"
    varConstructor (SmartPtr "Buffer") "buffer" $ new "Buffer" []
    map_foreach "i" ("s"~>"streamData") $ do
      switch (value "i"~>"tag") $ do
        case_ (ns ["ArrayValue","tag","VBoolArray"]) $ do
          varADT "VBoolArray" "a" $ value "i"
        case_ (ns ["ArrayValue","tag","VIntArray"]) $ do
          varADT "VIntArray" "a" $ value "i"
        case_ (ns ["ArrayValue","tag","VWordArray"]) $ do
          varADT "VWordArray" "a" $ value "i"
        case_ (ns ["ArrayValue","tag","VFloatArray"]) $ do
          varADT "VFloatArray" "a" $ value "i"
        varAssign (SmartPtr "ArrayValue") "type" $ callExp "inputType" ["s"~>"streamType" `mapLookup` key "i"]
        call ("gls"~>"streams"."add") [key "i", "type", "buffer", callExp ("buffer"~>"add") ["a"~>"_0"]]
    call ("buffer"~>"freeze") []
    call ("gls"~>"streams"."validate") []

    "gls"~>"glCount" .= 0
    map_foreach "i" ("gls"~>"streams"."map") $ do
      if_ (value "i"~>"isArray") $ do
        then_ $ do
          ("gls"~>"glCount") .= ((value "i"~>"buffer"~>"size" `vectorLookup` value "i"~>"index") / value "i"~>"glSize")
          break_
    return_ "gls"
{-
  procedure "createProgram" ["p_" :. SmartPtr "Program"] (SmartPtr "GLProgram") $ do
    varADT "Program" "p" "p_"
    -- vertex shader
    varAssign UInt "vs" $ call "glCreateShader" ["GL_VERTEX_SHADER"]
    charPtrFromString "vsSrc" $ "p"~>"vertexShader"
    call "glShaderSource" ["vs",1,addr "vsSrc",nullptr]
    call "glCompileShader" ["vs"]

    -- fragment shader
    varAssign UInt "fs" $ call "glCreateShader" ["GL_FRAGMENT_SHADER"]
    charPtrFromString "fsSrc" $ "p"~>"fragmentShader"
    call "glShaderSource" ["fs",1,addr "fsSrc",nullptr]
    call "glCompileShader" ["fs"]

    -- create program
    varAssign UInt "po" $ call "glCreateProgram" []
    call "glAttachShader" ["po","vs"]
    call "glAttachShader" ["po","fs"]
    call "glLinkProgram" ["po"]

    varConstructor (SmartPtr "GLProgram") "glp" $ new $ "GLProgram" []
    "glp"~>"program" .= "po"
    "glp"~>"vertexShader" .= "vs"
    "glp"~>"fragmentShader" .= "fs"

    -- query uniforms
    var Int ["loc"]
    map_foreach "i" ("p"~>"programUniforms") $ do
      "loc" .= call "glGetUniformLocation" ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ do
        "glp"~>"programUniforms" `mapLookup` (key "i") .= "loc"

    -- query sampler uniforms
    map_foreach "i" ("p"~>"programInTextures") $ do
      "loc" .= call "glGetUniformLocation" ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ do
        "glp"~>"programInTextures" `mapLookup` (key "i") .= "loc"
    -- query vertex attributes
    map_foreach "i" ("p"~>"programStreams") $ do
      "loc" .= call "glGetAttribLocation" ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ do
        varADT "Parameter" "param" $ value "i"
        "glp"~>"programStreams" `mapLookup` (key "i") .= recordValue [("name","param"~>"name"),("index","loc")]
    return_ "glp"

  procedure "setupRasterContext" ["ctx_" :. SmartPtr "RasterContext"] Void $ do
    switch ("ctx_"~>"tag") $ do
      case_ (ns ["RasterContext","tag","PointCtx"]) $ do
        varADT "PointCtx" "ctx" "ctx_"
        switch ("ctx"~>"_0"~>"tag") $ do
          case_ (ns ["PointSize","tag","ProgramPointSize"]) $ return ()
          default_ $ do
            throw "unsupported point size"
      case_ (ns ["RasterContext","tag","LineCtx"]) $ do
        varADT "LineCtx" "ctx" "ctx_"
        call "glLineWidth" ["ctx"~>"_0"]
      case_ (ns ["RasterContext","tag","TriangleCtx"]) $ do
        varADT "TriangleCtx" "ctx" "ctx_"
        switch ("ctx"~>"_0"~>"tag") $ do
          case_ (ns ["CullMode","tag","CullNone"]) $ do
            call "glDisable" ["GL_CULL_FACE"]
          case_ (ns ["CullMode","tag","CullFront"]) $ do
            varADT "CullFront" "f" $ "ctx"~>"_0"
            call "glEnable" ["GL_CULL_FACE"]
            call "glCullFace" ["GL_FRONT"]
            call "glFrontFace" [call "frontFace" ["f"~>"_0"]]
          case_ (ns ["CullMode","tag","CullBack"]) $ do
            varADT "CullBack" "f" $ "ctx"~>"_0"
            call "glEnable" ["GL_CULL_FACE"]
            call "glCullFace" ["GL_BACK"]
            call "glFrontFace" [call "frontFace" ["f"~>"_0"]]
        call "glDisable" ["GL_POLYGON_OFFSET_FILL"]
        switch ("ctx"~>"_2"~>"tag") $ do
          case_ (ns ["PolygonOffset","tag","NoOffset"]) $ return () -- TODO
          case_ (ns ["PolygonOffset","tag","Offset"]) $ do
            varADT "Offset" "o" $ "ctx"~>"_2"
            call "glPolygonOffset" ["o"~>"_0","o"~>"_1"]
            call "glEnable" ["GL_POLYGON_OFFSET_FILL"]

  procedure "setupAccumulationContext" ["ctx_" :. SmartPtr "AccumulationContext"] Void $ do
    varADT "AccumulationContext" "ctx" "ctx_"
    varAssign Bool "noDepth" true
    varAssign Bool "noStencil" true
    varAssign Bool "noColor" true
    vector_foreach "i" ("ctx"~>"accOperations") $ do
      switch ("i"~>"tag") $ do
        case_ (ns ["FragmentOperation","tag","DepthOp"]) $ do
          varADT "DepthOp" "o" "i"
          "noDepth" .= false
          varAssign Int "df" $ call "comparisonFunction" ["o"~>"_0"]
          if_ ("df" == "GL_ALWAYS" && "o"~>"_1" == false) $ do
            then_ $ call "glDisable" ["GL_DEPTH_TEST"]
            else_ $ do
              call "glEnable" ["GL_DEPTH_TEST"]
              call "glDepthFunc" ["df"]
              call "glDepthMask" ["o"~>"_1"]

        case_ (ns ["FragmentOperation","tag","StencilOp"]) $ do
          varADT "StencilOp" "o" "i"
          "noStencil" .= false

        case_ (ns ["FragmentOperation","tag","ColorOp"]) $ do
          varADT "ColorOp" "o" "i"
          "noColor" .= false
          switch ("o"~>"_0"~>"tag") $ do
            case_ (ns ["Blending","tag","NoBlending"]) $ do
              call "glDisable" ["GL_BLEND"]
            case_ (ns ["Blending","tag","BlendLogicOp"]) $ do
              call "glDisable" ["GL_BLEND"]
            case_ (ns ["Blending","tag","Blend"]) $ do
              varADT "Blend" "b" $ "o"~>"_0"
              call "glEnable" ["GL_BLEND"]
              call "glBlendEquationSeparate" [call "blendEquation" ["b"~>"colorEqSrc"], call "blendEquation" ["b"~>"alphaEqSrc"]]
              call "glBlendColor" ["b"~>"color"."x","b"~>"color"."y","b"~>"color"."z","b"~>"color"."w"]
              call "glBlendFuncSeparate" [ call "blendingFactor" ["b"~>"colorFSrc"], call "blendingFactor" ["b"~>"colorFDst"]
                                         , call "blendingFactor" ["b"~>"alphaFSrc"], call "blendingFactor" ["b"~>"alphaFDst"]
                                         ]
          varAssign Bool "maskR" true
          varAssign Bool "maskG" true
          varAssign Bool "maskB" true
          varAssign Bool "maskA" true
          switch ("o"~>"_1"~>"tag") $ do
            case_ (ns ["Value","tag","VBool"]) $ do
              varADT "VBool" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"
            case_ (ns ["Value","tag","VV2B"]) $ do
              varADT "VV2B" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"."x"
              "maskG" .= "v"~>"_0"."y"
            case_ (ns ["Value","tag","VV3B"]) $ do
              varADT "VV3B" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"."x"
              "maskG" .= "v"~>"_0"."y"
              "maskB" .= "v"~>"_0"."z"
            case_ (ns ["Value","tag","VV4B"]) $ do
              varADT "VV4B" "v" $ "o"~>"_1"
              "maskR" .= "v"~>"_0"."x"
              "maskG" .= "v"~>"_0"."y"
              "maskB" .= "v"~>"_0"."z"
              "maskA" .= "v"~>"_0"."w"
          call "glColorMask" ["maskR","maskG","maskB","maskA"]
    if_ "noStencil" $ then_ $ call "glDisable" ["GL_STENCIL_TEST"]
    if_ "noDepth" $ then_ $ call "glDisable" ["GL_DEPTH_TEST"]

pipelineMethods = do
  method "GLES20Pipeline" "createRenderTarget" ["t_" :. SmartPtr "RenderTarget"] UInt $ do
    varADT "RenderTarget" "t" "t_"
    -- does this target have texture attachments?
    varAssign Int "textureCount" 0
    vector_foreach "i_" ("t"~>"renderTargets") $ do
      varADT "TargetItem" "i" "i_"
      if_ ("i"~>"targetRef"."valid" && "i"~>"targetRef"."data"~>"tag" == ns ["ImageRef","tag","TextureImage"]) $ do
        inc "textureCount"
    if_ ("textureCount" == 0)
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
        case_ (ns ["ImageSemantic","tag","Color"]) $ do 
          "attachment" .= "GL_COLOR_ATTACHMENT0"
        case_ (ns ["ImageSemantic","tag","Depth"]) $ do 
          "attachment" .= "GL_DEPTH_ATTACHMENT"
        case_ (ns ["ImageSemantic","tag","Stencil"]) $ do 
          "attachment" .= "GL_STENCIL_ATTACHMENT"
      if_ ("i"~>"targetRef"."valid") $ do
        then_ $ switch ("i"~>"targetRef"."data"~>"tag") $ do
          case_ (ns ["ImageRef","tag","TextureImage"]) $ do
            varADT "TextureImage" "ti" $ "i"~>"targetRef"."data"
            "texture" .= ("textures" `vectorLookup` ("ti"~>"_0"))."texture"
            "textarget" .= "GL_TEXTURE_2D" -- TODO
            "level" .= "ti"~>"_1"
          case_ (ns ["ImageRef","tag","Framebuffer"]) $ do
            "texture" .= 0
            "textarget" .= "GL_TEXTURE_2D"
            "level" .= 0
        else_ $ do
          "texture" .= 0
          "textarget" .= "GL_TEXTURE_2D"
          "level" .= 0
      call "glFramebufferTexture2D" ["GL_FRAMEBUFFER","attachment","textarget","texture","level"]
    return_ "fb"

  constructor "GLES20Pipeline" ["ppl_" :. SmartPtr "Pipeline"] $ do
    "screenTarget" .= 0
    "hasCurrentProgram" .= false
    varADT "Pipeline" "ppl" $ "ppl_"
    "pipeline" .= "ppl"
    -- check backend compatibility
    if_ ("ppl"~>"backend"~>"tag" != ns ["Backend","tag","WebGL1"]) $ do
      throw "unsupported backend"
    -- allocate all resources
    --  textures
    vector_foreach "i" ("ppl"~>"textures") $ do
      vector_pushBack "textures" $ call "createTexture" ["i"]
    --  targets
    vector_foreach "i" ("ppl"~>"targets") $ do
      vector_pushBack "targets" $ call "createRenderTarget" ["i"]
    --  programs
    vector_foreach "i" ("ppl"~>"programs") $ do
      vector_pushBack "programs" $ call "createProgram" ["i"]
    --  stream data
    vector_foreach "i" ("ppl"~>"streams") $ do
      vector_pushBack "streamData" $ call "createStreamData" ["i"]
    call "glReleaseShaderCompiler" []

  destructor "GLES20Pipeline" $ do
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

  method "GLES20Pipeline" "setPipelineInput" ["i" :. SmartPtr "PipelineInput"] Void $ do
    "input" .= "i"

  method "GLES20Pipeline" "render" [] Void $ do
    vector_foreach "i" ("pipeline"~>"commands") $ do
      switch ("i"~>"tag") $ do
        case_ (ns ["Command","tag","SetRasterContext"]) $ do
          varADT "SetRasterContext" "cmd" $ "i"
          call "setupRasterContext" ["cmd"~>"_0"]
        case_ (ns ["Command","tag","SetAccumulationContext"]) $ do
          varADT "SetAccumulationContext" "cmd" $ "i"
          call "setupAccumulationContext" ["cmd"~>"_0"]
        case_ (ns ["Command","tag","SetTexture"]) $ do
          varADT "SetTexture" "cmd" $ "i"
          call "glActiveTexture" ["GL_TEXTURE0" + "cmd"~>"_0"]
          call "glBindTexture" [("textures" `vectorLookup` ("cmd"~>"_1"))."target",("textures" `vectorLookup` ("cmd"~>"_1"))."texture"]
        case_ (ns ["Command","tag","SetProgram"]) $ do
          varADT "SetProgram" "cmd" $ "i"
          "hasCurrentProgram" .= true
          "currentProgram" .= "cmd"~>"_0"
          call "glUseProgram" [("programs" `vectorLookup` "currentProgram")~>"program"]
        case_ (ns ["Command","tag","SetRenderTarget"]) $ do
          varADT "SetRenderTarget" "cmd" $ "i"
          varAssign UInt "t" $ "targets" `vectorLookup` ("cmd"~>"_0")
          call "glBindFramebuffer" ["GL_FRAMEBUFFER", expIf ("t"==0) "screenTarget" "t"]
          if_ (notNull "input") $ do
            then_ $ call "glViewport" [0,0,"input"~>"screenWidth","input"~>"screenHeight"]
        case_ (ns ["Command","tag","ClearRenderTarget"]) $ do
          varADT "ClearRenderTarget" "cmd" $ "i"
          varAssign UInt "mask" 0
          vector_foreach "a" ("cmd"~>"_0") $ do
            varADT "ClearImage" "image" $ "a"
            switch ("image"~>"imageSemantic"~>"tag") $ do
              case_ (ns ["ImageSemantic","tag","Depth"]) $ do
                varADT "VFloat" "v" $ "image"~>"clearValue"
                call "glDepthMask" [true]
                call "glClearDepthf" ["v"~>"_0"]
                "mask" |= "GL_DEPTH_BUFFER_BIT"
              case_ (ns ["ImageSemantic","tag","Stencil"]) $ do
                varADT "VWord" "v" $ "image"~>"clearValue"
                call "glClearStencil" ["v"~>"_0"]
                "mask" |= "GL_STENCIL_BUFFER_BIT"
              case_ (ns ["ImageSemantic","tag","Color"]) $ do
                switch ("image"~>"clearValue"~>"tag") $ do
                  case_ (ns ["Value","tag","VFloat"]) $ do
                    varADT "VFloat" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0",0.0,0.0,1.0]
                  case_ (ns ["Value","tag","VV2F"]) $ do
                    varADT "VV2F" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0"."x","v"~>"_0"."y",0.0,1.0]
                  case_ (ns ["Value","tag","VV3F"]) $ do
                    varADT "VV3F" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0"."x","v"~>"_0"."y","v"~>"_0"."z",1.0]
                  case_ (ns ["Value","tag","VV4F"]) $ do
                    varADT "VV4F" "v" $ "image"~>"clearValue"
                    call "glClearColor" ["v"~>"_0"."x","v"~>"_0"."y","v"~>"_0"."z","v"~>"_0"."w"]
                  default_ $ do
                    call "glClearColor" [0.0,0.0,0.0,1.0]
                call "glColorMask" [true,true,true,true]
                "mask" |= "GL_COLOR_BUFFER_BIT"
          call "glClear" ["mask"]
        case_ (ns ["Command","tag","SetSamplerUniform"]) $  if_ "hasCurrentProgram" $ then_ $ do
          varADT "SetSamplerUniform" "cmd" $ "i"
          varAssign Int "sampler" $ ("programs" `vectorLookup` "currentProgram")~>"programInTextures" `mapLookup` ("cmd"~>"_0")
          call "glUniform1i" ["sampler","cmd"~>"_1"]
        case_ (ns ["Command","tag","RenderSlot"]) $ if_ (notNull "input" && notNull "pipeline" && "hasCurrentProgram") $ then_ $ do
          varADT "RenderSlot" "cmd" $ "i"
          varADT "Slot" "slot" $ "pipeline"~>"slots" `vectorLookup` ("cmd"~>"_0")
          if_ (map_notElem ("input"~>"objectMap") ("slot"~>"slotName")) $ then_ break_
          map_foreach "o" (deref $ "input"~>"objectMap" `mapLookup` ("slot"~>"slotName")) $ do
            if_ (not $ "o"~>"enabled") $ then_ continue_
            -- setup uniforms
            map_foreach "u" (("programs" `vectorLookup` "currentProgram")~>"programUniforms") $ do
              if_ (map_elem ("o"~>"uniforms") (key "u")) $ do
                then_ $ call "setUniformValue" [value "u","o"~>"uniforms" `mapLookup` (key "u")]
                else_ $ call "setUniformValue" [value "u","input"~>"uniforms" `mapLookup` (key "u")]
            -- setup streams
            map_foreach "s" (("programs" `vectorLookup` "currentProgram")~>"programStreams") $ do
              call "setStream" [value "s"."index",deref $ "o"~>"streams"~>"map" `mapLookup` (value "s"."name")]
            -- draw call
            -- TODO: support index buffers
            call "glDrawArrays" ["o"~>"glMode", 0, "o"~>"glCount"]
        case_ (ns ["Command","tag","RenderStream"]) $ if_ (notNull "input" && notNull "pipeline" && "hasCurrentProgram") $ then_ $ do
          varADT "RenderStream" "cmd" $ "i"
          varAssign (SmartPtr "ArrayValue") "data" $ "streamData" `vectorLookup` ("cmd"~>"_0")
          -- setup streams
          map_foreach "s" (("programs" `vectorLookup` "currentProgram")~>"programStreams") $ do
            call "setStream" [value "s"."index",deref $ "data"~>"streams"~>"map" `mapLookup` (value "s"."name")]
          -- draw call
          -- TODO: support index buffers
          call "glDrawArrays" ["data"~>"glMode", 0, "data"~>"glCount"]

backend = do
  enumConversions
  globalFunctions
  pipelineMethods
-}