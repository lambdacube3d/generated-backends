package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Util {
  static public Type inputType(InputType t) throws Exception {
    switch (t.tag) {
      case Float: return Type.FLOAT;
      case V2F: return Type.FLOAT_VEC2;
      case V3F: return Type.FLOAT_VEC3;
      case V4F: return Type.FLOAT_VEC4;
      case M22F: return Type.FLOAT_MAT2;
      case M33F: return Type.FLOAT_MAT3;
      case M44F: return Type.FLOAT_MAT4;
    }
    throw new Exception("illegal input type");
  }

  static public Integer primitiveMode(Primitive p) throws Exception {
    switch (p) {
      case TriangleStrip: return GLES20.GL_TRIANGLE_STRIP;
      case TriangleList: return GLES20.GL_TRIANGLES;
      case TriangleFan: return GLES20.GL_TRIANGLE_FAN;
      case LineStrip: return GLES20.GL_LINE_STRIP;
      case LineList: return GLES20.GL_LINES;
      case LineLoop: return GLES20.GL_LINE_LOOP;
      case PointList: return GLES20.GL_POINTS;
    }
    throw new Exception("illegal primitive mode");
  }

  static public Integer blendingFactor(BlendingFactor bf) throws Exception {
    switch (bf.tag) {
      case ConstantAlpha: return GLES20.GL_CONSTANT_ALPHA;
      case ConstantColor: return GLES20.GL_CONSTANT_COLOR;
      case DstAlpha: return GLES20.GL_DST_ALPHA;
      case DstColor: return GLES20.GL_DST_COLOR;
      case One: return GLES20.GL_ONE;
      case OneMinusConstantAlpha: return GLES20.GL_ONE_MINUS_CONSTANT_ALPHA;
      case OneMinusConstantColor: return GLES20.GL_ONE_MINUS_CONSTANT_COLOR;
      case OneMinusDstAlpha: return GLES20.GL_ONE_MINUS_DST_ALPHA;
      case OneMinusDstColor: return GLES20.GL_ONE_MINUS_DST_COLOR;
      case OneMinusSrcAlpha: return GLES20.GL_ONE_MINUS_SRC_ALPHA;
      case OneMinusSrcColor: return GLES20.GL_ONE_MINUS_SRC_COLOR;
      case SrcAlpha: return GLES20.GL_SRC_ALPHA;
      case SrcAlphaSaturate: return GLES20.GL_SRC_ALPHA_SATURATE;
      case SrcColor: return GLES20.GL_SRC_COLOR;
      case Zero: return GLES20.GL_ZERO;
    }
    throw new Exception("illegal blending factor");
  }

  static public Integer blendEquation(BlendEquation be) throws Exception {
    switch (be.tag) {
      case FuncAdd: return GLES20.GL_FUNC_ADD;
      case FuncReverseSubtract: return GLES20.GL_FUNC_REVERSE_SUBTRACT;
      case FuncSubtract: return GLES20.GL_FUNC_SUBTRACT;
    }
    throw new Exception("illegal blend equation");
  }

  static public Integer comparisonFunction(ComparisonFunction cf) throws Exception {
    switch (cf.tag) {
      case Always: return GLES20.GL_ALWAYS;
      case Equal: return GLES20.GL_EQUAL;
      case Gequal: return GLES20.GL_GEQUAL;
      case Greater: return GLES20.GL_GREATER;
      case Lequal: return GLES20.GL_LEQUAL;
      case Less: return GLES20.GL_LESS;
      case Never: return GLES20.GL_NEVER;
      case Notequal: return GLES20.GL_NOTEQUAL;
    }
    throw new Exception("illegal comparison function");
  }

  static public Integer frontFace(FrontFace ff) throws Exception {
    switch (ff.tag) {
      case CCW: return GLES20.GL_CCW;
      case CW: return GLES20.GL_CW;
    }
    throw new Exception("illegal front face value");
  }

  static public Integer textureDataTypeToGLType(ImageSemantic s_, TextureDataType d_) throws Exception {
    switch (s_.tag) {
      case Color: return GLES20.GL_RGBA;
      case Depth: return GLES20.GL_DEPTH_COMPONENT;
    }
    throw new Exception("FIXME: This texture format is not yet supported");
  }

  static public Integer textureDataTypeToGLArityType(ImageSemantic s_, TextureDataType d_) throws Exception {
    switch (s_.tag) {
      case Color: return GLES20.GL_RGBA;
      case Depth: return GLES20.GL_DEPTH_COMPONENT;
    }
    throw new Exception("FIXME: This texture format is not yet supported");
  }

  static public Integer edgeMode(EdgeMode e) throws Exception {
    switch (e.tag) {
      case ClampToEdge: return GLES20.GL_CLAMP_TO_EDGE;
      case Repeat: return GLES20.GL_REPEAT;
      case MirroredRepeat: return GLES20.GL_MIRRORED_REPEAT;
      default:
        throw new Exception("unsupported edge mode");

    }
  }

  static public Integer filterMode(Filter f) throws Exception {
    switch (f.tag) {
      case Nearest: return GLES20.GL_NEAREST;
      case Linear: return GLES20.GL_LINEAR;
      case NearestMipmapNearest: return GLES20.GL_NEAREST_MIPMAP_NEAREST;
      case NearestMipmapLinear: return GLES20.GL_NEAREST_MIPMAP_LINEAR;
      case LinearMipmapNearest: return GLES20.GL_LINEAR_MIPMAP_NEAREST;
      case LinearMipmapLinear: return GLES20.GL_LINEAR_MIPMAP_LINEAR;
      default:
        throw new Exception("unsupported filter mode");

    }
  }

  static public void setUniformValue(Integer i, UniformValue v) throws Exception {
    switch (v.tag) {
      case Int: {
        GLES20.glUniform1iv(i, 1, v._int, 0);
        break;
      }
      case Bool: {
        GLES20.glUniform1iv(i, 1, v._int, 0);
        break;
      }
      case Float: {
        GLES20.glUniform1fv(i, 1, v._float, 0);
        break;
      }
      case V2I: {
        GLES20.glUniform2iv(i, 1, v._int, 0);
        break;
      }
      case V2B: {
        GLES20.glUniform2iv(i, 1, v._int, 0);
        break;
      }
      case V2F: {
        GLES20.glUniform2fv(i, 1, v._float, 0);
        break;
      }
      case V3I: {
        GLES20.glUniform3iv(i, 1, v._int, 0);
        break;
      }
      case V3B: {
        GLES20.glUniform3iv(i, 1, v._int, 0);
        break;
      }
      case V3F: {
        GLES20.glUniform3fv(i, 1, v._float, 0);
        break;
      }
      case V4I: {
        GLES20.glUniform4iv(i, 1, v._int, 0);
        break;
      }
      case V4B: {
        GLES20.glUniform4iv(i, 1, v._int, 0);
        break;
      }
      case V4F: {
        GLES20.glUniform4fv(i, 1, v._float, 0);
        break;
      }
      case M22F: {
        GLES20.glUniformMatrix2fv(i, 1, false, v._float, 0);
        break;
      }
      case M33F: {
        GLES20.glUniformMatrix3fv(i, 1, false, v._float, 0);
        break;
      }
      case M44F: {
        GLES20.glUniformMatrix4fv(i, 1, false, v._float, 0);
        break;
      }
    }
  }

  static public void setStream(Integer i, Stream s) throws Exception {
    if (s.isArray) {
      GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, s.buffer.bufferObject);
      GLES20.glEnableVertexAttribArray(i);
      GLES20.glVertexAttribPointer(i, s.glSize, s.buffer.glType.get(s.index), false, 0, s.buffer.offset.get(s.index));
    } else {
      GLES20.glDisableVertexAttribArray(i);
      switch (s.type) {
        case FLOAT: {
          GLES20.glVertexAttrib1fv(i, s.attributeValue._float, 0);
          break;
        }
        case FLOAT_VEC2: {
          GLES20.glVertexAttrib2fv(i, s.attributeValue._float, 0);
          break;
        }
        case FLOAT_VEC3: {
          GLES20.glVertexAttrib3fv(i, s.attributeValue._float, 0);
          break;
        }
        case FLOAT_VEC4: {
          GLES20.glVertexAttrib4fv(i, s.attributeValue._float, 0);
          break;
        }
        case FLOAT_MAT2: {
          GLES20.glVertexAttrib2fv(i, s.attributeValue._float, 0);
          GLES20.glVertexAttrib2fv(i + 1, s.attributeValue._float, 2);
          break;
        }
        case FLOAT_MAT3: {
          GLES20.glVertexAttrib3fv(i, s.attributeValue._float, 0);
          GLES20.glVertexAttrib3fv(i + 1, s.attributeValue._float, 3);
          GLES20.glVertexAttrib3fv(i + 2, s.attributeValue._float, 6);
          break;
        }
        case FLOAT_MAT4: {
          GLES20.glVertexAttrib4fv(i, s.attributeValue._float, 0);
          GLES20.glVertexAttrib4fv(i + 1, s.attributeValue._float, 4);
          GLES20.glVertexAttrib4fv(i + 2, s.attributeValue._float, 8);
          GLES20.glVertexAttrib4fv(i + 3, s.attributeValue._float, 12);
          break;
        }
      }
    }
  }

  static public Texture createTexture(TextureDescriptor tx_) throws Exception {
    Texture t = new Texture();
    { int[] glObj = new int[1]; GLES20.glGenTextures(1, glObj, 0); t.texture = glObj[0]; }
    TextureDescriptor.TextureDescriptor_ tx = (TextureDescriptor.TextureDescriptor_)tx_;
    Value.VV2U_ size = (Value.VV2U_)tx.textureSize;
    Integer width = size._0.x;
    Integer height = size._0.y;
    Integer internalFormat, dataFormat;
    switch (tx.textureType.tag) {
      case Texture2D: {
        t.target = GLES20.GL_TEXTURE_2D;
        TextureType.Texture2D_ tx2D = (TextureType.Texture2D_)tx.textureType;
        internalFormat = textureDataTypeToGLType(tx.textureSemantic, tx2D._0);
        dataFormat = textureDataTypeToGLArityType(tx.textureSemantic, tx2D._0);
        break;
      }
      case TextureCube: {
        t.target = GLES20.GL_TEXTURE_CUBE_MAP;
        TextureType.TextureCube_ txCube = (TextureType.TextureCube_)tx.textureType;
        internalFormat = textureDataTypeToGLType(tx.textureSemantic, txCube._0);
        dataFormat = textureDataTypeToGLArityType(tx.textureSemantic, txCube._0);
        break;
      }
      default:
        throw new Exception("unsupported texture type");

    }
    GLES20.glBindTexture(t.target, t.texture);
    Integer dataType = dataFormat == GLES20.GL_DEPTH_COMPONENT?GLES20.GL_UNSIGNED_SHORT:GLES20.GL_UNSIGNED_BYTE;
    for (Integer level = tx.textureBaseLevel; level <= tx.textureMaxLevel; level++ ) {
      if (t.target == GLES20.GL_TEXTURE_2D) {
        GLES20.glTexImage2D(t.target, level, internalFormat, width, height, 0, dataFormat, dataType, null);
      } else {
        GLES20.glTexImage2D(GLES20.GL_TEXTURE_CUBE_MAP_POSITIVE_X, level, internalFormat, width, height, 0, dataFormat, dataType, null);
        GLES20.glTexImage2D(GLES20.GL_TEXTURE_CUBE_MAP_NEGATIVE_X, level, internalFormat, width, height, 0, dataFormat, dataType, null);
        GLES20.glTexImage2D(GLES20.GL_TEXTURE_CUBE_MAP_POSITIVE_Y, level, internalFormat, width, height, 0, dataFormat, dataType, null);
        GLES20.glTexImage2D(GLES20.GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, level, internalFormat, width, height, 0, dataFormat, dataType, null);
        GLES20.glTexImage2D(GLES20.GL_TEXTURE_CUBE_MAP_POSITIVE_Z, level, internalFormat, width, height, 0, dataFormat, dataType, null);
        GLES20.glTexImage2D(GLES20.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, level, internalFormat, width, height, 0, dataFormat, dataType, null);
      }
      width /= 2;
      height /= 2;
    }
    SamplerDescriptor.SamplerDescriptor_ s = (SamplerDescriptor.SamplerDescriptor_)tx.textureSampler;
    GLES20.glTexParameteri(t.target, GLES20.GL_TEXTURE_WRAP_S, edgeMode(s.samplerWrapS));
    GLES20.glTexParameteri(t.target, GLES20.GL_TEXTURE_WRAP_T, edgeMode(s.samplerWrapT.data));
    GLES20.glTexParameteri(t.target, GLES20.GL_TEXTURE_MIN_FILTER, filterMode(s.samplerMinFilter));
    GLES20.glTexParameteri(t.target, GLES20.GL_TEXTURE_MAG_FILTER, filterMode(s.samplerMagFilter));
    return t;
  }

  static public GLStreamData createStreamData(StreamData s_) throws Exception {
    StreamData.StreamData_ s = (StreamData.StreamData_)s_;
    GLStreamData gls = new GLStreamData();
    switch (s.streamPrimitive.tag) {
      case Points: gls.glMode = GLES20.GL_POINTS; break;
      case Lines: gls.glMode = GLES20.GL_LINES; break;
      case Triangles: gls.glMode = GLES20.GL_TRIANGLES; break;
    }
    GLBuffer buffer = new GLBuffer();
    for (Map.Entry<String,ArrayValue> i : s.streamData.entrySet()) {
      switch (i.getValue().tag) {
        case VBoolArray: {
          ArrayValue.VBoolArray_ a = (ArrayValue.VBoolArray_)i.getValue();
          break;
        }
        case VIntArray: {
          ArrayValue.VIntArray_ a = (ArrayValue.VIntArray_)i.getValue();
          break;
        }
        case VWordArray: {
          ArrayValue.VWordArray_ a = (ArrayValue.VWordArray_)i.getValue();
          break;
        }
        case VFloatArray: {
          ArrayValue.VFloatArray_ a = (ArrayValue.VFloatArray_)i.getValue();
          Type type = inputType(s.streamType.get(i.getKey()));
          FloatBuffer buf = FloatBuffer.allocate(a._0.size());
          for (Float vec_elem : a._0) buf.put(vec_elem);
          buf.rewind();
          gls.streams.add(i.getKey(), type, buffer, buffer.add(buf, GLES20.GL_FLOAT, a._0.size()));
          break;
        }
      }
    }
    buffer.freeze();
    gls.streams.validate();
    gls.glCount = 0;
    for (Map.Entry<String,Stream> i : gls.streams.map.entrySet()) {
      if (i.getValue().isArray) {
        gls.glCount = i.getValue().buffer.size.get(i.getValue().index) / i.getValue().glSize;
        break;
      }
    }
    return gls;
  }

  static public GLProgram createProgram(Program p_) throws Exception {
    Program.Program_ p = (Program.Program_)p_;
    Integer vs = GLES20.glCreateShader(GLES20.GL_VERTEX_SHADER);
    GLES20.glShaderSource(vs, p.vertexShader);
    GLES20.glCompileShader(vs);
    Integer fs = GLES20.glCreateShader(GLES20.GL_FRAGMENT_SHADER);
    GLES20.glShaderSource(fs, p.fragmentShader);
    GLES20.glCompileShader(fs);
    Integer po = GLES20.glCreateProgram();
    GLES20.glAttachShader(po, vs);
    GLES20.glAttachShader(po, fs);
    GLES20.glLinkProgram(po);
    GLProgram glp = new GLProgram();
    glp.program = po;
    glp.vertexShader = vs;
    glp.fragmentShader = fs;
    Integer loc;
    for (Map.Entry<String,InputType> i : p.programUniforms.entrySet()) {
      loc = GLES20.glGetUniformLocation(po, i.getKey());
      if (loc >= 0) {
        glp.programUniforms.put(i.getKey(), loc);
      }
    }
    for (Map.Entry<String,InputType> i : p.programInTextures.entrySet()) {
      loc = GLES20.glGetUniformLocation(po, i.getKey());
      if (loc >= 0) {
        glp.programInTextures.put(i.getKey(), loc);
      }
    }
    for (Map.Entry<String,Parameter> i : p.programStreams.entrySet()) {
      loc = GLES20.glGetAttribLocation(po, i.getKey());
      if (loc >= 0) {
        Parameter.Parameter_ param = (Parameter.Parameter_)i.getValue();
        StreamInfo s = new StreamInfo();
        s.name = param.name;
        s.index = loc;
        glp.programStreams.put(i.getKey(), s);
      }
    }
    return glp;
  }

  static public void setupRasterContext(RasterContext ctx_) throws Exception {
    switch (ctx_.tag) {
      case PointCtx: {
        RasterContext.PointCtx_ ctx = (RasterContext.PointCtx_)ctx_;
        switch (ctx._0.tag) {
          case ProgramPointSize: {
            break;
          }
          default:
            throw new Exception("unsupported point size");

        }
        break;
      }
      case LineCtx: {
        RasterContext.LineCtx_ ctx = (RasterContext.LineCtx_)ctx_;
        GLES20.glLineWidth(ctx._0);
        break;
      }
      case TriangleCtx: {
        RasterContext.TriangleCtx_ ctx = (RasterContext.TriangleCtx_)ctx_;
        switch (ctx._0.tag) {
          case CullNone: GLES20.glDisable(GLES20.GL_CULL_FACE); break;
          case CullFront: {
            CullMode.CullFront_ f = (CullMode.CullFront_)ctx._0;
            GLES20.glEnable(GLES20.GL_CULL_FACE);
            GLES20.glCullFace(GLES20.GL_FRONT);
            GLES20.glFrontFace(frontFace(f._0));
            break;
          }
          case CullBack: {
            CullMode.CullBack_ f = (CullMode.CullBack_)ctx._0;
            GLES20.glEnable(GLES20.GL_CULL_FACE);
            GLES20.glCullFace(GLES20.GL_BACK);
            GLES20.glFrontFace(frontFace(f._0));
            break;
          }
        }
        GLES20.glDisable(GLES20.GL_POLYGON_OFFSET_FILL);
        switch (ctx._2.tag) {
          case NoOffset: {
            break;
          }
          case Offset: {
            PolygonOffset.Offset_ o = (PolygonOffset.Offset_)ctx._2;
            GLES20.glPolygonOffset(o._0, o._1);
            GLES20.glEnable(GLES20.GL_POLYGON_OFFSET_FILL);
            break;
          }
        }
        break;
      }
    }
  }

  static public void setupAccumulationContext(AccumulationContext ctx_) throws Exception {
    AccumulationContext.AccumulationContext_ ctx = (AccumulationContext.AccumulationContext_)ctx_;
    Boolean noDepth = true;
    Boolean noStencil = true;
    Boolean noColor = true;
    for (FragmentOperation i : ctx.accOperations) {
      switch (i.tag) {
        case DepthOp: {
          FragmentOperation.DepthOp_ o = (FragmentOperation.DepthOp_)i;
          noDepth = false;
          Integer df = comparisonFunction(o._0);
          if (df == GLES20.GL_ALWAYS && o._1 == false) {
            GLES20.glDisable(GLES20.GL_DEPTH_TEST);
          } else {
            GLES20.glEnable(GLES20.GL_DEPTH_TEST);
            GLES20.glDepthFunc(df);
            GLES20.glDepthMask(o._1);
          }
          break;
        }
        case StencilOp: {
          FragmentOperation.StencilOp_ o = (FragmentOperation.StencilOp_)i;
          noStencil = false;
          break;
        }
        case ColorOp: {
          FragmentOperation.ColorOp_ o = (FragmentOperation.ColorOp_)i;
          noColor = false;
          switch (o._0.tag) {
            case NoBlending: GLES20.glDisable(GLES20.GL_BLEND); break;
            case BlendLogicOp: GLES20.glDisable(GLES20.GL_BLEND); break;
            case Blend: {
              Blending.Blend_ b = (Blending.Blend_)o._0;
              GLES20.glEnable(GLES20.GL_BLEND);
              GLES20.glBlendEquationSeparate(blendEquation(b.colorEqSrc), blendEquation(b.alphaEqSrc));
              GLES20.glBlendColor(b.color.x, b.color.y, b.color.z, b.color.w);
              GLES20.glBlendFuncSeparate(blendingFactor(b.colorFSrc), blendingFactor(b.colorFDst), blendingFactor(b.alphaFSrc), blendingFactor(b.alphaFDst));
              break;
            }
          }
          Boolean maskR = true;
          Boolean maskG = true;
          Boolean maskB = true;
          Boolean maskA = true;
          switch (o._1.tag) {
            case VBool: {
              Value.VBool_ v = (Value.VBool_)o._1;
              maskR = v._0;
              break;
            }
            case VV2B: {
              Value.VV2B_ v = (Value.VV2B_)o._1;
              maskR = v._0.x;
              maskG = v._0.y;
              break;
            }
            case VV3B: {
              Value.VV3B_ v = (Value.VV3B_)o._1;
              maskR = v._0.x;
              maskG = v._0.y;
              maskB = v._0.z;
              break;
            }
            case VV4B: {
              Value.VV4B_ v = (Value.VV4B_)o._1;
              maskR = v._0.x;
              maskG = v._0.y;
              maskB = v._0.z;
              maskA = v._0.w;
              break;
            }
          }
          GLES20.glColorMask(maskR, maskG, maskB, maskA);
          break;
        }
      }
    }
    if (noStencil) {
      GLES20.glDisable(GLES20.GL_STENCIL_TEST);
    }
    if (noDepth) {
      GLES20.glDisable(GLES20.GL_DEPTH_TEST);
    }
  }

}

