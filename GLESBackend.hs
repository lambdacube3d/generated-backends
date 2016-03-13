{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Prelude (($),Num (..),return)
import qualified Prelude
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
      case_ (nsPat ["Primitive","TriangleStrip"]) $ return_ $ toExp GL_TRIANGLE_STRIP
      case_ (nsPat ["Primitive","TriangleList"])  $ return_ $ toExp GL_TRIANGLES
      case_ (nsPat ["Primitive","TriangleFan"])   $ return_ $ toExp GL_TRIANGLE_FAN
      case_ (nsPat ["Primitive","LineStrip"])     $ return_ $ toExp GL_LINE_STRIP
      case_ (nsPat ["Primitive","LineList"])      $ return_ $ toExp GL_LINES
      case_ (nsPat ["Primitive","LineLoop"])      $ return_ $ toExp GL_LINE_LOOP
      case_ (nsPat ["Primitive","PointList"])     $ return_ $ toExp GL_POINTS

  procedure "blendingFactor" ["bf" :@ SmartPtr "BlendingFactor"] Int $ do
    switch ("bf"~>"tag") $ do
      case_ (nsPat ["BlendingFactor","tag","ConstantAlpha"]) $ return_ $ toExp GL_CONSTANT_ALPHA
      case_ (nsPat ["BlendingFactor","tag","ConstantColor"]) $ return_ $ toExp GL_CONSTANT_COLOR
      case_ (nsPat ["BlendingFactor","tag","DstAlpha"]) $ return_ $ toExp GL_DST_ALPHA
      case_ (nsPat ["BlendingFactor","tag","DstColor"]) $ return_ $ toExp GL_DST_COLOR
      case_ (nsPat ["BlendingFactor","tag","One"]) $ return_ $ toExp GL_ONE
      case_ (nsPat ["BlendingFactor","tag","OneMinusConstantAlpha"]) $ return_ $ toExp GL_ONE_MINUS_CONSTANT_ALPHA
      case_ (nsPat ["BlendingFactor","tag","OneMinusConstantColor"]) $ return_ $ toExp GL_ONE_MINUS_CONSTANT_COLOR
      case_ (nsPat ["BlendingFactor","tag","OneMinusDstAlpha"]) $ return_ $ toExp GL_ONE_MINUS_DST_ALPHA
      case_ (nsPat ["BlendingFactor","tag","OneMinusDstColor"]) $ return_ $ toExp GL_ONE_MINUS_DST_COLOR
      case_ (nsPat ["BlendingFactor","tag","OneMinusSrcAlpha"]) $ return_ $ toExp GL_ONE_MINUS_SRC_ALPHA
      case_ (nsPat ["BlendingFactor","tag","OneMinusSrcColor"]) $ return_ $ toExp GL_ONE_MINUS_SRC_COLOR
      case_ (nsPat ["BlendingFactor","tag","SrcAlpha"]) $ return_ $ toExp GL_SRC_ALPHA
      case_ (nsPat ["BlendingFactor","tag","SrcAlphaSaturate"]) $ return_ $ toExp GL_SRC_ALPHA_SATURATE
      case_ (nsPat ["BlendingFactor","tag","SrcColor"]) $ return_ $ toExp GL_SRC_COLOR
      case_ (nsPat ["BlendingFactor","tag","Zero"]) $ return_ $ toExp GL_ZERO
    throw "illegal blending factor"

  procedure "blendEquation" ["be" :@ SmartPtr "BlendEquation"] Int $ do
    switch ("be"~>"tag") $ do
      case_ (nsPat ["BlendEquation","tag","FuncAdd"]) $ return_ $ toExp GL_FUNC_ADD
      case_ (nsPat ["BlendEquation","tag","FuncReverseSubtract"]) $ return_ $ toExp GL_FUNC_REVERSE_SUBTRACT
      case_ (nsPat ["BlendEquation","tag","FuncSubtract"]) $ return_ $ toExp GL_FUNC_SUBTRACT
    throw "illegal blend equation"

  procedure "comparisonFunction" ["cf" :@ SmartPtr "ComparisonFunction"] Int $ do
    switch ("cf"~>"tag") $ do
      case_ (nsPat ["ComparisonFunction","tag","Always"]) $ return_ $ toExp GL_ALWAYS
      case_ (nsPat ["ComparisonFunction","tag","Equal"]) $ return_ $ toExp GL_EQUAL
      case_ (nsPat ["ComparisonFunction","tag","Gequal"]) $ return_ $ toExp GL_GEQUAL
      case_ (nsPat ["ComparisonFunction","tag","Greater"]) $ return_ $ toExp GL_GREATER
      case_ (nsPat ["ComparisonFunction","tag","Lequal"]) $ return_ $ toExp GL_LEQUAL
      case_ (nsPat ["ComparisonFunction","tag","Less"]) $ return_ $ toExp GL_LESS
      case_ (nsPat ["ComparisonFunction","tag","Never"]) $ return_ $ toExp GL_NEVER
      case_ (nsPat ["ComparisonFunction","tag","Notequal"]) $ return_ $ toExp GL_NOTEQUAL
    throw "illegal comparison function"

  procedure "frontFace" ["ff" :@ SmartPtr "FrontFace"] Int $ do
    switch ("ff"~>"tag") $ do
      case_ (nsPat ["FrontFace","tag","CCW"]) $ return_ $ toExp GL_CCW
      case_ (nsPat ["FrontFace","tag","CW"]) $ return_ $ toExp GL_CW
    throw "illegal front face value"

  procedure "textureDataTypeToGLType" ["s_" :@ SmartPtr "ImageSemantic", "d_" :@ SmartPtr "TextureDataType"] Int $ do
    switch ("s_"~>"tag") $ do
      case_ (nsPat ["ImageSemantic","tag","Color"]) $ return_ $ toExp GL_RGBA
      case_ (nsPat ["ImageSemantic","tag","Depth"]) $ return_ $ toExp GL_DEPTH_COMPONENT
    throw "FIXME: This texture format is not yet supported"

  procedure "textureDataTypeToGLArityType" ["s_" :@ SmartPtr "ImageSemantic", "d_" :@ SmartPtr "TextureDataType"] Int $ do
    switch ("s_"~>"tag") $ do
      case_ (nsPat ["ImageSemantic","tag","Color"]) $ return_ $ toExp GL_RGBA
      case_ (nsPat ["ImageSemantic","tag","Depth"]) $ return_ $ toExp GL_DEPTH_COMPONENT
    throw "FIXME: This texture format is not yet supported"

  procedure "edgeMode" ["e" :@ SmartPtr "EdgeMode"] Int $ do
    switch ("e"~>"tag") $ do
      case_ (nsPat ["EdgeMode","tag","ClampToEdge"]) $ return_ $ toExp GL_CLAMP_TO_EDGE
      case_ (nsPat ["EdgeMode","tag","Repeat"]) $ return_ $ toExp GL_REPEAT
      case_ (nsPat ["EdgeMode","tag","MirroredRepeat"]) $ return_ $ toExp GL_MIRRORED_REPEAT
      default_ $ throw "unsupported edge mode"

  procedure "filterMode" ["f" :@ SmartPtr "Filter"] Int $ do
    switch ("f"~>"tag") $ do
      case_ (nsPat ["Filter","tag","Nearest"]) $ return_ $ toExp GL_NEAREST
      case_ (nsPat ["Filter","tag","Linear"]) $ return_ $ toExp GL_LINEAR
      case_ (nsPat ["Filter","tag","NearestMipmapNearest"]) $ return_ $ toExp GL_NEAREST_MIPMAP_NEAREST
      case_ (nsPat ["Filter","tag","NearestMipmapLinear"]) $ return_ $ toExp GL_NEAREST_MIPMAP_LINEAR
      case_ (nsPat ["Filter","tag","LinearMipmapNearest"]) $ return_ $ toExp GL_LINEAR_MIPMAP_NEAREST
      case_ (nsPat ["Filter","tag","LinearMipmapLinear"]) $ return_ $ toExp GL_LINEAR_MIPMAP_LINEAR
      default_ $ throw "unsupported filter mode"

globalFunctions = do
  procedure "setUniformValue" ["i" :@ Int, "v" :@ Ref "UniformValue"] Void $ do
    switch ("v"."tag") $ do
      case_ (nsPat ["InputType","tag","Int"])    $ callGL GLUniform1i ["i","v"."_int"]
      case_ (nsPat ["InputType","tag","Word"])   $ callGL GLUniform1i ["i","v"."_word"]
      case_ (nsPat ["InputType","tag","Float"])  $ callGL GLUniform1f ["i","v"."_float"]
      case_ (nsPat ["InputType","tag","Bool"])   $ callGL GLUniform1i ["i","v"."_bool"]
      case_ (nsPat ["InputType","tag","V2I"])    $ callGL GLUniform2iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v2i"]
      case_ (nsPat ["InputType","tag","V2U"])    $ callGL GLUniform2iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v2u"]
      case_ (nsPat ["InputType","tag","V2F"])    $ callGL GLUniform2fv ["i",1,cast (Ptr Float) $ addr $ "v"."_v2f"]
      case_ (nsPat ["InputType","tag","V2B"])    $ callGL GLUniform2iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v2b"]

      case_ (nsPat ["InputType","tag","V3I"])    $ callGL GLUniform3iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v3i"]
      case_ (nsPat ["InputType","tag","V3U"])    $ callGL GLUniform3iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v3u"]
      case_ (nsPat ["InputType","tag","V3F"])    $ callGL GLUniform3fv ["i",1,cast (Ptr Float) $ addr $ "v"."_v3f"]
      case_ (nsPat ["InputType","tag","V3B"])    $ callGL GLUniform3iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v3b"]

      case_ (nsPat ["InputType","tag","V4I"])    $ callGL GLUniform4iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v4i"]
      case_ (nsPat ["InputType","tag","V4U"])    $ callGL GLUniform4iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v4u"]
      case_ (nsPat ["InputType","tag","V4F"])    $ callGL GLUniform4fv ["i",1,cast (Ptr Float) $ addr $ "v"."_v4f"]
      case_ (nsPat ["InputType","tag","V4B"])    $ callGL GLUniform4iv ["i",1,cast (Ptr Int) $ addr $ "v"."_v4b"]

      case_ (nsPat ["InputType","tag","M22F"])   $ callGL GLUniformMatrix2fv ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m22f"]
      case_ (nsPat ["InputType","tag","M33F"])   $ callGL GLUniformMatrix3fv ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m33f"]
      case_ (nsPat ["InputType","tag","M44F"])   $ callGL GLUniformMatrix4fv ["i",1,false,cast (Ptr Float) $ addr $ "v"."_m44f"]

  procedure "setStream" ["i" :@ Int, "s" :@ Ref "Stream"] Void $ do
    if_ ("s"."isArray") $ do
      then_ $ do
        callGL GLBindBuffer [toExp GL_ARRAY_BUFFER,"s"."buffer"~>"bufferObject"]
        callGL GLEnableVertexAttribArray ["i"]
        callGL GLVertexAttribPointer
          [ "i", "s"."glSize"
          , ("s"."buffer"~>"glType") `vector_lookup` ("s"."index"), false, 0
          , cast (Const $ Ptr Void) $ ("s"."buffer"~>"offset") `vector_lookup` ("s"."index")
          ]
      else_ $ do
        callGL GLDisableVertexAttribArray ["i"]
        switch ("s"."type") $ do
          case_ (nsPat ["Type","FLOAT"]) $ callGL GLVertexAttrib1f ["i","s"."_float"]
          case_ (nsPat ["Type","FLOAT_VEC2"]) $ callGL GLVertexAttrib2fv ["i", cast (Ptr Float) $ addr $ "s"."_v2f"]
          case_ (nsPat ["Type","FLOAT_VEC3"]) $ callGL GLVertexAttrib3fv ["i", cast (Ptr Float) $ addr $ "s"."_v3f"]
          case_ (nsPat ["Type","FLOAT_VEC4"]) $ callGL GLVertexAttrib4fv ["i", cast (Ptr Float) $ addr $ "s"."_v4f"]
          case_ (nsPat ["Type","FLOAT_MAT2"]) $ do callGL GLVertexAttrib2fv ["i",     cast (Ptr Float) $ addr $ "s"."_m22f"."x"]
                                                   callGL GLVertexAttrib2fv ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m22f"."y"]
          case_ (nsPat ["Type","FLOAT_MAT3"]) $ do callGL GLVertexAttrib3fv ["i",     cast (Ptr Float) $ addr $ "s"."_m33f"."x"]
                                                   callGL GLVertexAttrib3fv ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m33f"."y"]
                                                   callGL GLVertexAttrib3fv ["i" + 2, cast (Ptr Float) $ addr $ "s"."_m33f"."z"]
          case_ (nsPat ["Type","FLOAT_MAT4"]) $ do callGL GLVertexAttrib4fv ["i",     cast (Ptr Float) $ addr $ "s"."_m44f"."x"]
                                                   callGL GLVertexAttrib4fv ["i" + 1, cast (Ptr Float) $ addr $ "s"."_m44f"."y"]
                                                   callGL GLVertexAttrib4fv ["i" + 2, cast (Ptr Float) $ addr $ "s"."_m44f"."z"]
                                                   callGL GLVertexAttrib4fv ["i" + 3, cast (Ptr Float) $ addr $ "s"."_m44f"."w"]

  procedure "createTexture" ["tx_" :@ SmartPtr "TextureDescriptor"] "Texture" $ do
    var "Texture" ["t"]
    callGL GLGenTextures [1,addr $ "t"."texture"]
    varADT "TextureDescriptor" "tx" "tx_"
    varADT "VV2U" "size" $ "tx"~>"textureSize"
    varAssign Int "width" $ "size"~>"_0"."x"
    varAssign Int "height" $ "size"~>"_0"."y"
    var Int ["internalFormat","dataFormat"]
    switch ("tx"~>"textureType"~>"tag") $ do
      case_ (nsPat ["TextureType","tag","Texture2D"]) $ do
        "t"."target" .= toExp GL_TEXTURE_2D
        varADT "Texture2D" "tx2D" $ "tx"~>"textureType"
        "internalFormat" .= callExp "textureDataTypeToGLType" ["tx"~>"textureSemantic","tx2D"~>"_0"]
        "dataFormat" .= callExp "textureDataTypeToGLArityType" ["tx"~>"textureSemantic","tx2D"~>"_0"]
      case_ (nsPat ["TextureType","tag","TextureCube"]) $ do
        "t"."target" .= toExp GL_TEXTURE_CUBE_MAP
        varADT "TextureCube" "txCube" $ "tx"~>"textureType"
        "internalFormat" .= callExp "textureDataTypeToGLType" ["tx"~>"textureSemantic","txCube"~>"_0"]
        "dataFormat" .= callExp "textureDataTypeToGLArityType" ["tx"~>"textureSemantic","txCube"~>"_0"]
      default_ $ throw "unsupported texture type"

    callGL GLBindTexture ["t"."target", "t"."texture"]
    varAssign Int "dataType" $ expIf ("dataFormat" == toExp GL_DEPTH_COMPONENT) (toExp GL_UNSIGNED_SHORT) (toExp GL_UNSIGNED_BYTE)
    for (varAssign Int "level" $ "tx"~>"textureBaseLevel") ("level" <= "tx"~>"textureMaxLevel") (incExp "level") $ do
      if_ ("t"."target" == toExp GL_TEXTURE_2D) $ do
        then_ $ do
          callGL GLTexImage2D ["t"."target","level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
        else_ $ do
          callGL GLTexImage2D [toExp GL_TEXTURE_CUBE_MAP_POSITIVE_X,"level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          callGL GLTexImage2D [toExp GL_TEXTURE_CUBE_MAP_NEGATIVE_X,"level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          callGL GLTexImage2D [toExp GL_TEXTURE_CUBE_MAP_POSITIVE_Y,"level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          callGL GLTexImage2D [toExp GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,"level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          callGL GLTexImage2D [toExp GL_TEXTURE_CUBE_MAP_POSITIVE_Z,"level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
          callGL GLTexImage2D [toExp GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,"level","internalFormat", "width", "height", 0, "dataFormat", "dataType", nullptr]
      "width" /= 2
      "height" /= 2

    -- setup texture sampling
    varADT "SamplerDescriptor" "s" $ "tx"~>"textureSampler"
    callGL GLTexParameteri ["t"."target", toExp GL_TEXTURE_WRAP_S, callExp "edgeMode" ["s"~>"samplerWrapS"]]
    callGL GLTexParameteri ["t"."target", toExp GL_TEXTURE_WRAP_T, callExp "edgeMode" ["s"~>"samplerWrapT"."data"]]
    callGL GLTexParameteri ["t"."target", toExp GL_TEXTURE_MIN_FILTER, callExp "filterMode" ["s"~>"samplerMinFilter"]]
    callGL GLTexParameteri ["t"."target", toExp GL_TEXTURE_MAG_FILTER, callExp "filterMode" ["s"~>"samplerMagFilter"]]
    return_ "t"

  procedure "createStreamData" ["s_" :@ SmartPtr "StreamData"] (SmartPtr "GLStreamData") $ do
    varADT "StreamData" "s" "s_"
    varConstructor (SmartPtr "GLStreamData") "gls" $ new "GLStreamData" []

    switch ("s"~>"streamPrimitive"~>"tag") $ do
      case_ (nsPat ["FetchPrimitive","tag","Points"])    $ "gls"~>"glMode" .= toExp GL_POINTS
      case_ (nsPat ["FetchPrimitive","tag","Lines"])     $ "gls"~>"glMode" .= toExp GL_LINES
      case_ (nsPat ["FetchPrimitive","tag","Triangles"]) $ "gls"~>"glMode" .= toExp GL_TRIANGLES
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
    varAssign UInt "vs" $ callExpGL GLCreateShader [toExp GL_VERTEX_SHADER]
    varCharPtrFromString "vsSrc" $ "p"~>"vertexShader"
    callGL GLShaderSource ["vs",1,addr "vsSrc",nullptr]
    callGL GLCompileShader ["vs"]

    -- fragment shader
    varAssign UInt "fs" $ callExpGL GLCreateShader [toExp GL_FRAGMENT_SHADER]
    varCharPtrFromString "fsSrc" $ "p"~>"fragmentShader"
    callGL GLShaderSource ["fs",1,addr "fsSrc",nullptr]
    callGL GLCompileShader ["fs"]

    -- create program
    varAssign UInt "po" $ callExpGL GLCreateProgram []
    callGL GLAttachShader ["po","vs"]
    callGL GLAttachShader ["po","fs"]
    callGL GLLinkProgram ["po"]

    varConstructor (SmartPtr "GLProgram") "glp" $ new "GLProgram" []
    "glp"~>"program" .= "po"
    "glp"~>"vertexShader" .= "vs"
    "glp"~>"fragmentShader" .= "fs"

    -- query uniforms
    var Int ["loc"]
    map_foreach "i" ("p"~>"programUniforms") $ do
      "loc" .= callExpGL GLGetUniformLocation ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ then_ $ do
        "glp"~>"programUniforms" `map_lookup` (key "i") .= "loc"

    -- query sampler uniforms
    map_foreach "i" ("p"~>"programInTextures") $ do
      "loc" .= callExpGL GLGetUniformLocation ["po", charPtrFromString $ key "i"]
      if_ ("loc" >= 0) $ then_ $ do
        "glp"~>"programInTextures" `map_lookup` (key "i") .= "loc"
    -- query vertex attributes
    map_foreach "i" ("p"~>"programStreams") $ do
      "loc" .= callExpGL GLGetAttribLocation ["po", charPtrFromString $ key "i"]
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
        callGL GLLineWidth ["ctx"~>"_0"]
      case_ (nsPat ["RasterContext","tag","TriangleCtx"]) $ do
        varADT "TriangleCtx" "ctx" "ctx_"
        switch ("ctx"~>"_0"~>"tag") $ do
          case_ (nsPat ["CullMode","tag","CullNone"]) $ do
            callGL GLDisable [toExp GL_CULL_FACE]
          case_ (nsPat ["CullMode","tag","CullFront"]) $ do
            varADT "CullFront" "f" $ "ctx"~>"_0"
            callGL GLEnable [toExp GL_CULL_FACE]
            callGL GLCullFace [toExp GL_FRONT]
            callGL GLFrontFace [callExp "frontFace" ["f"~>"_0"]]
          case_ (nsPat ["CullMode","tag","CullBack"]) $ do
            varADT "CullBack" "f" $ "ctx"~>"_0"
            callGL GLEnable [toExp GL_CULL_FACE]
            callGL GLCullFace [toExp GL_BACK]
            callGL GLFrontFace [callExp "frontFace" ["f"~>"_0"]]
        callGL GLDisable [toExp GL_POLYGON_OFFSET_FILL]
        switch ("ctx"~>"_2"~>"tag") $ do
          case_ (nsPat ["PolygonOffset","tag","NoOffset"]) $ return () -- TODO
          case_ (nsPat ["PolygonOffset","tag","Offset"]) $ do
            varADT "Offset" "o" $ "ctx"~>"_2"
            callGL GLPolygonOffset ["o"~>"_0","o"~>"_1"]
            callGL GLEnable [toExp GL_POLYGON_OFFSET_FILL]

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
          if_ ("df" == toExp GL_ALWAYS && "o"~>"_1" == false) $ do
            then_ $ callGL GLDisable [toExp GL_DEPTH_TEST]
            else_ $ do
              callGL GLEnable [toExp GL_DEPTH_TEST]
              callGL GLDepthFunc ["df"]
              callGL GLDepthMask ["o"~>"_1"]

        case_ (nsPat ["FragmentOperation","tag","StencilOp"]) $ do
          varADT "StencilOp" "o" "i"
          "noStencil" .= false

        case_ (nsPat ["FragmentOperation","tag","ColorOp"]) $ do
          varADT "ColorOp" "o" "i"
          "noColor" .= false
          switch ("o"~>"_0"~>"tag") $ do
            case_ (nsPat ["Blending","tag","NoBlending"]) $ do
              callGL GLDisable [toExp GL_BLEND]
            case_ (nsPat ["Blending","tag","BlendLogicOp"]) $ do
              callGL GLDisable [toExp GL_BLEND]
            case_ (nsPat ["Blending","tag","Blend"]) $ do
              varADT "Blend" "b" $ "o"~>"_0"
              callGL GLEnable [toExp GL_BLEND]
              callGL GLBlendEquationSeparate [callExp "blendEquation" ["b"~>"colorEqSrc"], callExp "blendEquation" ["b"~>"alphaEqSrc"]]
              callGL GLBlendColor ["b"~>"color"."x","b"~>"color"."y","b"~>"color"."z","b"~>"color"."w"]
              callGL GLBlendFuncSeparate [ callExp "blendingFactor" ["b"~>"colorFSrc"], callExp "blendingFactor" ["b"~>"colorFDst"]
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
          callGL GLColorMask ["maskR","maskG","maskB","maskA"]
    if_ "noStencil" $ then_ $ callGL GLDisable [toExp GL_STENCIL_TEST]
    if_ "noDepth" $ then_ $ callGL GLDisable [toExp GL_DEPTH_TEST]

classes = do
  enum_ "Primitive"
    [ "TriangleStrip"
    , "TriangleList"
    , "TriangleFan"
    , "LineStrip"
    , "LineList"
    , "LineLoop"
    , "PointList"
    ]

  enum_ "Type"
    [ "FLOAT"
    , "FLOAT_VEC2"
    , "FLOAT_VEC3"
    , "FLOAT_VEC4"
    , "FLOAT_MAT2"
    , "FLOAT_MAT3"
    , "FLOAT_MAT4"
    ]

  class_ "Buffer" $ do
    public $ do
      classVar (Vector Int) ["size","byteSize","glType"]
      classVar (Vector Long) ["offset"]
      classVar (Vector (Ptr Void)) ["data"]
      classVar UInt ["bufferObject"]

      method "add" ["v" :@ Ref (Vector Int8)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" (vector_dataPtr "v")
        vector_pushBack "size" (vector_size "v")
        vector_pushBack "byteSize" (vector_size "v")
        vector_pushBack "glType" $ toExp GL_BYTE
        return_ "i"

      method "add" ["v" :@ Ref (Vector UInt8)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" (vector_dataPtr "v")
        vector_pushBack "size" (vector_size "v")
        vector_pushBack "byteSize" (vector_size "v")
        vector_pushBack "glType" $ toExp GL_UNSIGNED_BYTE
        return_ "i"

      method "add" ["v" :@ Ref (Vector Int16)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" (vector_dataPtr "v")
        vector_pushBack "size" (vector_size "v")
        vector_pushBack "byteSize" (2 * vector_size "v")
        vector_pushBack "glType" $ toExp GL_SHORT
        return_ "i"

      method "add" ["v" :@ Ref (Vector UInt16)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" (vector_dataPtr "v")
        vector_pushBack "size" (vector_size "v")
        vector_pushBack "byteSize" (2 * vector_size "v")
        vector_pushBack "glType" $ toExp GL_UNSIGNED_SHORT
        return_ "i"

      method "add" ["v" :@ Ref (Vector Float)] Int $ do
        varAssign Int "i" $ vector_size "data"
        vector_pushBack "data" (vector_dataPtr "v")
        vector_pushBack "size" (vector_size "v")
        vector_pushBack "byteSize" (4 * vector_size "v")
        vector_pushBack "glType" $ toExp GL_FLOAT
        return_ "i"

      method "freeze" [] Void $ do
        varAssign UInt "bufferSize" 0
        vector_foreach "i" "byteSize" $ do
          vector_pushBack "offset" "bufferSize"
          "bufferSize" += "i"

        var UInt ["bo"]
        callGL GLGenBuffers [1,addr "bo"]
        callGL GLBindBuffer [toExp GL_ARRAY_BUFFER,"bo"]
        callGL GLBufferData [toExp GL_ARRAY_BUFFER,"bufferSize",nullptr,toExp GL_STATIC_DRAW]
        varAssign UInt "offset_" 0
        for_range "i" 0 (vector_size "data") $ do
          callGL GLBufferSubData [toExp GL_ARRAY_BUFFER,"offset_","byteSize" `vector_lookup` "i","data" `vector_lookup` "i"]
          "offset_" += ("byteSize" `vector_lookup` "i")
        "bufferObject" .= "bo"

      method "update" ["i" :@ Int, "v" :@ Ref (Vector Float)] Void $ return () -- TODO

  class_ "Stream" $ do
    public $ do
      classVar "Type" ["type"]
      classVar (SmartPtr "Buffer") ["buffer"]
      classVar Int ["index"]
      classVar Bool ["isArray"]
      classVar Int ["glSize"]
      classUnion
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
      constructor ["b" :@ SmartPtr "Buffer", "i" :@ Int, "t" :@ "Type"] $ do
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
      classVar (Map String (SmartPtr "Stream")) ["map"]

      method "add" ["name" :@ String, "v" :@ Ref Float] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "V2F"] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "V3F"] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "V4F"] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "M22F"] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "M33F"] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "v" :@ Ref "M44F"] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["v"]
      method "add" ["name" :@ String, "t" :@ "Type", "b" :@ SmartPtr "Buffer", "index" :@ Int] Void $ map_insert "map" "name" $ callTypeConsructor (SmartPtr "Stream") $ new "Stream" ["b","index","t"]
      method "validate" [] Bool $ return_ true -- TODO

  struct_ "UniformValue" $ do
    structVar (Enum "InputType::tag") ["tag"] -- TODO
    structUnion
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
      classVar Bool ["enabled"]
      classVar Int ["order","glMode","glCount"]
      classVar (Map String "UniformValue") ["uniforms"]
      classVar (SmartPtr "StreamMap") ["streams"]

      destructor $ return () -- TODO

      method "enable" ["visible" :@ Bool] Void $ "enabled" .= "visible"
      method "setOrder" ["o" :@ Int] Void $ "order" .= "o"

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
      classVar (Map String (SmartPtr (Vector (SmartPtr "Object")))) ["objectMap"]
      classVar (Map String "UniformValue") ["uniforms"]
      classVar Int ["screenWidth","screenHeight"]

      method "createObject" ["slotName" :@ String, "prim" :@ "Primitive", "attributes" :@ SmartPtr "StreamMap", "objectUniforms" :@ Vector String] (SmartPtr "Object") $ do
        varConstructor (SmartPtr "Object") "o" $ new "Object" []
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
            vector_pushBackPtr ("objectMap" `map_lookup` "slotName") "o"
          else_ $ do
            varAssign (Ptr $ Vector $ SmartPtr "Object") "ov" $ new (Vector $ SmartPtr "Object") []
            vector_pushBackPtr "ov" "o"
            map_insert "objectMap" "slotName" $ CallTypeConsructor (SmartPtr $ Vector $ SmartPtr "Object") "ov"
        return_ "o"

      method "createObject" ["slotName" :@ String, "prim" :@ "Primitive", "attributes" :@ Ref "StreamMap"
                            , "indexBuffer" :@ Ref "Buffer", "bufferIndex" :@ Int, "objectUniforms" :@ Vector String] (SmartPtr "Object") $ do
        varConstructor (SmartPtr "Object") "o" $ new "Object" []
        -- TODO
        return_ "o" 

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
    structVar Int ["target"]
    structVar UInt ["texture"]

  struct_ "StreamInfo" $ do
    structVar String ["name"]
    structVar Int ["index"]

  class_ "GLProgram" $ do
    public $ do
      classVar UInt ["program","vertexShader","fragmentShader"]
      classVar (Map String Int) ["programUniforms","programInTextures"]
      classVar (Map String "StreamInfo") ["programStreams"]

  struct_ "GLStreamData" $ do
    structVar Int ["glMode","glCount"]
    structVar "StreamMap" ["streams"]

  class_ "GLES20Pipeline" $ do
    private $ do
      classVar (SmartPtr "PipelineInput") ["input"]
      classVar (SmartPtr "data::Pipeline") ["pipeline"] -- TODO: type namespace
      classVar (Vector "Texture") ["textures"]
      classVar (Vector UInt) ["targets"]
      classVar (Vector (SmartPtr "GLProgram")) ["programs"]
      classVar (Vector (SmartPtr "GLStreamData")) ["streamData"]
      classVar UInt ["currentProgram"]
      classVar Bool ["hasCurrentProgram"]

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
        callGL GLGenFramebuffers [1,addr "fb"]
        callGL GLBindFramebuffer [toExp GL_FRAMEBUFFER, "fb"]
        var Int ["attachment","textarget","level"]
        var UInt ["texture"]
        vector_foreach "i_" ("t"~>"renderTargets") $ do
          varADT "TargetItem" "i" "i_"
          switch ("i"~>"targetSemantic"~>"tag") $ do
            case_ (nsPat ["ImageSemantic","tag","Color"]) $ do 
              "attachment" .= toExp GL_COLOR_ATTACHMENT0
            case_ (nsPat ["ImageSemantic","tag","Depth"]) $ do 
              "attachment" .= toExp GL_DEPTH_ATTACHMENT
            case_ (nsPat ["ImageSemantic","tag","Stencil"]) $ do 
              "attachment" .= toExp GL_STENCIL_ATTACHMENT
          if_ ("i"~>"targetRef"."valid") $ do
            then_ $ switch ("i"~>"targetRef"."data"~>"tag") $ do
              case_ (nsPat ["ImageRef","tag","TextureImage"]) $ do
                varADT "TextureImage" "ti" $ "i"~>"targetRef"."data"
                "texture" .= ("textures" `vector_lookup` ("ti"~>"_0"))."texture"
                "textarget" .= toExp GL_TEXTURE_2D -- TODO
                "level" .= "ti"~>"_1"
              case_ (nsPat ["ImageRef","tag","Framebuffer"]) $ do
                "texture" .= 0
                "textarget" .= toExp GL_TEXTURE_2D
                "level" .= 0
            else_ $ do
              "texture" .= 0
              "textarget" .= toExp GL_TEXTURE_2D
              "level" .= 0
          callGL GLFramebufferTexture2D [toExp GL_FRAMEBUFFER,"attachment","textarget","texture","level"]
        return_ "fb"

    public $ do
      classVar UInt ["screenTarget"]

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
          vector_pushBack "textures" (callExp "createTexture" ["i"])
        --  targets
        vector_foreach "i" ("ppl"~>"targets") $ do
          vector_pushBack "targets" (callExp "createRenderTarget" ["i"])
        --  programs
        vector_foreach "i" ("ppl"~>"programs") $ do
          vector_pushBack "programs" (callExp "createProgram" ["i"])
        --  stream data
        vector_foreach "i" ("ppl"~>"streams") $ do
          vector_pushBack "streamData" (callExp "createStreamData" ["i"])
        callGL GLReleaseShaderCompiler []

      destructor $ do
        -- release resources
        -- textures
        vector_foreach "i" "textures" $ do
          callGL GLDeleteTextures [1,addr "i"."texture"]
        -- targets
        vector_foreach "i" "targets" $ do
          callGL GLDeleteFramebuffers [1,addr "i"]
        -- programs
        vector_foreach "i" "programs" $ do
          callGL GLDeleteProgram ["i"~>"program"]
          callGL GLDeleteShader ["i"~>"vertexShader"]
          callGL GLDeleteShader ["i"~>"fragmentShader"]

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
              callGL GLActiveTexture [toExp GL_TEXTURE0 + "cmd"~>"_0"]
              callGL GLBindTexture [("textures" `vector_lookup` ("cmd"~>"_1"))."target",("textures" `vector_lookup` ("cmd"~>"_1"))."texture"]
            case_ (nsPat ["Command","tag","SetProgram"]) $ do
              varADT "SetProgram" "cmd" $ "i"
              "hasCurrentProgram" .= true
              "currentProgram" .= "cmd"~>"_0"
              callGL GLUseProgram [("programs" `vector_lookup` "currentProgram")~>"program"]
            case_ (nsPat ["Command","tag","SetRenderTarget"]) $ do
              varADT "SetRenderTarget" "cmd" $ "i"
              varAssign UInt "t" $ "targets" `vector_lookup` ("cmd"~>"_0")
              callGL GLBindFramebuffer [toExp GL_FRAMEBUFFER, expIf ("t"==0) "screenTarget" "t"]
              if_ (notNull "input") $ do
                then_ $ callGL GLViewport [0,0,"input"~>"screenWidth","input"~>"screenHeight"]
            case_ (nsPat ["Command","tag","ClearRenderTarget"]) $ do
              varADT "ClearRenderTarget" "cmd" $ "i"
              varAssign UInt "mask" 0
              vector_foreach "a" ("cmd"~>"_0") $ do
                varADT "ClearImage" "image" $ "a"
                switch ("image"~>"imageSemantic"~>"tag") $ do
                  case_ (nsPat ["ImageSemantic","tag","Depth"]) $ do
                    varADT "VFloat" "v" $ "image"~>"clearValue"
                    callGL GLDepthMask [true]
                    callGL GLClearDepthf ["v"~>"_0"]
                    "mask" |= toExp GL_DEPTH_BUFFER_BIT
                  case_ (nsPat ["ImageSemantic","tag","Stencil"]) $ do
                    varADT "VWord" "v" $ "image"~>"clearValue"
                    callGL GLClearStencil ["v"~>"_0"]
                    "mask" |= toExp GL_STENCIL_BUFFER_BIT
                  case_ (nsPat ["ImageSemantic","tag","Color"]) $ do
                    switch ("image"~>"clearValue"~>"tag") $ do
                      case_ (nsPat ["Value","tag","VFloat"]) $ do
                        varADT "VFloat" "v" $ "image"~>"clearValue"
                        callGL GLClearColor ["v"~>"_0",0.0,0.0,1.0]
                      case_ (nsPat ["Value","tag","VV2F"]) $ do
                        varADT "VV2F" "v" $ "image"~>"clearValue"
                        callGL GLClearColor ["v"~>"_0"."x","v"~>"_0"."y",0.0,1.0]
                      case_ (nsPat ["Value","tag","VV3F"]) $ do
                        varADT "VV3F" "v" $ "image"~>"clearValue"
                        callGL GLClearColor ["v"~>"_0"."x","v"~>"_0"."y","v"~>"_0"."z",1.0]
                      case_ (nsPat ["Value","tag","VV4F"]) $ do
                        varADT "VV4F" "v" $ "image"~>"clearValue"
                        callGL GLClearColor ["v"~>"_0"."x","v"~>"_0"."y","v"~>"_0"."z","v"~>"_0"."w"]
                      default_ $ do
                        callGL GLClearColor [0.0,0.0,0.0,1.0]
                    callGL GLColorMask [true,true,true,true]
                    "mask" |= toExp GL_COLOR_BUFFER_BIT
              callGL GLClear ["mask"]
            case_ (nsPat ["Command","tag","SetSamplerUniform"]) $  if_ "hasCurrentProgram" $ then_ $ do
              varADT "SetSamplerUniform" "cmd" $ "i"
              varAssign Int "sampler" $ ("programs" `vector_lookup` "currentProgram")~>"programInTextures" `map_lookup` ("cmd"~>"_0")
              callGL GLUniform1i ["sampler","cmd"~>"_1"]
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
                callGL GLDrawArrays ["o"~>"glMode", 0, "o"~>"glCount"]
            case_ (nsPat ["Command","tag","RenderStream"]) $ if_ (notNull "input" && notNull "pipeline" && "hasCurrentProgram") $ then_ $ do
              varADT "RenderStream" "cmd" $ "i"
              varAssign (SmartPtr "GLStreamData") "data" $ "streamData" `vector_lookup` ("cmd"~>"_0")
              -- setup streams
              map_foreach "s" (("programs" `vector_lookup` "currentProgram")~>"programStreams") $ do
                call "setStream" [it_value "s"."index",deref $ "data"~>"streams"."map" `map_lookup` (it_value "s"."name")]
              -- draw call
              -- TODO: support index buffers
              callGL GLDrawArrays ["data"~>"glMode", 0, "data"~>"glCount"]

backend = do
  enumConversions
  globalFunctions
  classes

main = do
  Prelude.writeFile "LambdaCube.hpp" $ prettyHpp backend
  Prelude.writeFile "LambdaCube.cpp" $ prettyCpp backend
