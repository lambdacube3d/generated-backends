#include <iostream>
#include <OpenGLES/ES2/gl.h>

#include "LambdaCube.hpp"

// more complex functions
void setUniformValue(int i, UniformValue& v) {
  switch (v.tag) {
    case InputType::tag::Int:   glUniform1i(i,v._int);              break;
    case InputType::tag::Word:  glUniform1i(i,v._word);             break;
    case InputType::tag::Float: glUniform1f(i,v._float);            break;
    case InputType::tag::Bool:  glUniform1i(i,v._bool);             break;
    case InputType::tag::V2I:   glUniform2iv(i,1,(int*)&v._v2i);    break;
    case InputType::tag::V2U:   glUniform2iv(i,1,(int*)&v._v2u);    break;
    case InputType::tag::V2F:   glUniform2fv(i,1,(float*)&v._v2f);  break;
    case InputType::tag::V2B:   glUniform2iv(i,1,(int*)&v._v2b);    break;
    case InputType::tag::V3I:   glUniform3iv(i,1,(int*)&v._v3i);    break;
    case InputType::tag::V3U:   glUniform3iv(i,1,(int*)&v._v3u);    break;
    case InputType::tag::V3F:   glUniform3fv(i,1,(float*)&v._v3f);  break;
    case InputType::tag::V3B:   glUniform3iv(i,1,(int*)&v._v3b);    break;
    case InputType::tag::V4I:   glUniform4iv(i,1,(int*)&v._v4i);    break;
    case InputType::tag::V4U:   glUniform4iv(i,1,(int*)&v._v4u);    break;
    case InputType::tag::V4F:   glUniform4fv(i,1,(float*)&v._v4f);  break;
    case InputType::tag::V4B:   glUniform4iv(i,1,(int*)&v._v4b);    break;
    case InputType::tag::M22F:  glUniformMatrix2fv(i,1,false,(float*)&v._m22f); break;
    case InputType::tag::M33F:  glUniformMatrix3fv(i,1,false,(float*)&v._m33f); break;
    case InputType::tag::M44F:  glUniformMatrix4fv(i,1,false,(float*)&v._m44f); break;
    default:;
  }
}

void setStream(int i, Stream& s) {
  if (s.isArray) {
    glBindBuffer(GL_ARRAY_BUFFER,s.buffer->bufferObject);
    glEnableVertexAttribArray(i);
    glVertexAttribPointer(i, s.glSize, s.buffer->glType[s.index], false, 0, (const void*)s.buffer->offset[s.index]);
  } else {
    glDisableVertexAttribArray(i);
    switch (s.type) {
      case Type::FLOAT:       glVertexAttrib1f(i,s._float);               break;
      case Type::FLOAT_VEC2:  glVertexAttrib2fv(i,  (float*)&s._v2f);     break;
      case Type::FLOAT_VEC3:  glVertexAttrib3fv(i,  (float*)&s._v3f);     break;
      case Type::FLOAT_VEC4:  glVertexAttrib4fv(i,  (float*)&s._v4f);     break;
      case Type::FLOAT_MAT2:  glVertexAttrib2fv(i,  (float*)&s._m22f.x);
                              glVertexAttrib2fv(i+1,(float*)&s._m22f.y);  break;
      case Type::FLOAT_MAT3:  glVertexAttrib3fv(i,  (float*)&s._m33f.x);
                              glVertexAttrib3fv(i+1,(float*)&s._m33f.y);
                              glVertexAttrib3fv(i+2,(float*)&s._m33f.z);  break;
      case Type::FLOAT_MAT4:  glVertexAttrib4fv(i,  (float*)&s._m44f.x);
                              glVertexAttrib4fv(i+1,(float*)&s._m44f.y);
                              glVertexAttrib4fv(i+2,(float*)&s._m44f.z);
                              glVertexAttrib4fv(i+3,(float*)&s._m44f.w);  break;
    }
  }
}

Texture createTexture(std::shared_ptr<TextureDescriptor> tx_) {
  Texture t;
  glGenTextures(1,&t.texture);
  auto tx = std::static_pointer_cast<data::TextureDescriptor>(tx_);
  auto size = std::static_pointer_cast<data::VV2U>(tx->textureSize);
  int width = size->_0.x;
  int height = size->_0.y;
  int internalFormat,dataFormat;
  switch (tx->textureType->tag) {
    case TextureType::tag::Texture2D: {
      t.target = GL_TEXTURE_2D;
      auto tx2D = std::static_pointer_cast<data::Texture2D>(tx->textureType);
      internalFormat = textureDataTypeToGLType(tx->textureSemantic,tx2D->_0);
      dataFormat = textureDataTypeToGLArityType(tx->textureSemantic,tx2D->_0);
      break;
    }
    case TextureType::tag::TextureCube: {
      t.target = GL_TEXTURE_CUBE_MAP;
      auto txCube = std::static_pointer_cast<data::TextureCube>(tx->textureType);
      internalFormat = textureDataTypeToGLType(tx->textureSemantic,txCube->_0);
      dataFormat = textureDataTypeToGLArityType(tx->textureSemantic,txCube->_0);
      break;
    }
    default:
      throw "unsupported texture type";
  }

  glBindTexture(t.target, t.texture);
  int dataType = (dataFormat == GL_DEPTH_COMPONENT)?GL_UNSIGNED_SHORT:GL_UNSIGNED_BYTE;
  for (int level = tx->textureBaseLevel; level <= tx->textureMaxLevel; level++) {
    if (t.target == GL_TEXTURE_2D) {
      glTexImage2D(t.target,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
    } else {
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,level,internalFormat, width, height, 0, dataFormat, dataType, nullptr);
    }
    width /= 2;
    height /= 2;
  }

  // setup texture sampling
  auto s = std::static_pointer_cast<data::SamplerDescriptor>(tx->textureSampler);
  glTexParameteri(t.target, GL_TEXTURE_WRAP_S, edgeMode(s->samplerWrapS));
  glTexParameteri(t.target, GL_TEXTURE_WRAP_T, edgeMode(s->samplerWrapT.data));
  glTexParameteri(t.target, GL_TEXTURE_MIN_FILTER, filterMode(s->samplerMinFilter));
  glTexParameteri(t.target, GL_TEXTURE_MAG_FILTER, filterMode(s->samplerMagFilter));
  return t;
}

std::shared_ptr<GLStreamData> createStreamData(std::shared_ptr<StreamData> s_) {
  auto s = std::static_pointer_cast<data::StreamData>(s_);
  std::shared_ptr<GLStreamData> gls(new GLStreamData());

  switch (s->streamPrimitive->tag) {
    case FetchPrimitive::tag::Points:    gls->glMode = GL_POINTS;    break;
    case FetchPrimitive::tag::Lines:     gls->glMode = GL_LINES;     break;
    case FetchPrimitive::tag::Triangles: gls->glMode = GL_TRIANGLES; break;
  }
  auto buffer = std::shared_ptr<Buffer>(new Buffer());
  for (auto i : s->streamData) {
    switch (i.second->tag) {
      case ArrayValue::tag::VBoolArray: {
        auto a = std::static_pointer_cast<data::VBoolArray>(i.second);
        // TODO
      }
      break;
      case ArrayValue::tag::VIntArray: {
        auto a = std::static_pointer_cast<data::VIntArray>(i.second);
        // TODO
        //auto type = inputType(s->streamType[i.first]);
        //gls->streams.add(i.first, type, buffer, buffer->add(a->_0));
      }
      break;
      case ArrayValue::tag::VWordArray: {
        auto a = std::static_pointer_cast<data::VWordArray>(i.second);
        // TODO
        //auto type = inputType(s->streamType[i.first]);
        //gls->streams.add(i.first, type, buffer, buffer->add(a->_0));
      }
      break;
      case ArrayValue::tag::VFloatArray: {
        auto a = std::static_pointer_cast<data::VFloatArray>(i.second);
        auto type = inputType(s->streamType[i.first]);
        gls->streams.add(i.first, type, buffer, buffer->add(a->_0));
      }
      break;
    }
  }
  buffer->freeze();
  gls->streams.validate(); // TODO

  gls->glCount = 0;
  for (auto i : gls->streams.map) {
    if (i.second->isArray) {
      gls->glCount = i.second->buffer->size[i.second->index] / i.second->glSize;
      break;
    }
  }
  return gls;
}

std::shared_ptr<GLProgram> createProgram(std::shared_ptr<Program> p_) {
  auto p = std::static_pointer_cast<data::Program>(p_);
  // vertex shader
  unsigned int vs = glCreateShader(GL_VERTEX_SHADER);
  const char* vsSrc = p->vertexShader.c_str();
  glShaderSource(vs,1,&vsSrc,nullptr);
  glCompileShader(vs);

  // fragment shader
  unsigned int fs = glCreateShader(GL_FRAGMENT_SHADER);
  const char* fsSrc = p->fragmentShader.c_str();
  glShaderSource(fs,1,&fsSrc,nullptr);
  glCompileShader(fs);

  // create program
  unsigned int po = glCreateProgram();
  glAttachShader(po,vs);
  glAttachShader(po,fs);
  glLinkProgram(po);

  std::shared_ptr<GLProgram> glp(new GLProgram());
  glp->program = po;
  glp->vertexShader = vs;
  glp->fragmentShader = fs;

  // query uniforms
  int loc;
  for(auto const &i : p->programUniforms) {
    loc = glGetUniformLocation(po, i.first.c_str());
    if (loc >= 0) {
      glp->programUniforms[i.first] = loc;
    }
  }
  // query sampler uniforms
  for(auto const &i : p->programInTextures) {
    loc = glGetUniformLocation(po, i.first.c_str());
    if (loc >= 0) {
      glp->programInTextures[i.first] = loc;
    }
  }
  // query vertex attributes
  for(auto const &i : p->programStreams) {
    loc = glGetAttribLocation(po, i.first.c_str());
    if (loc >= 0) {
      auto param = std::static_pointer_cast<data::Parameter>(i.second);
      glp->programStreams[i.first] = {.name = param->name, .index = loc};
    }
  }
  return glp;
}

void setupRasterContext(std::shared_ptr<RasterContext> ctx_) {
  switch (ctx_->tag) {
    case RasterContext::tag::PointCtx: {
      auto ctx = std::static_pointer_cast<data::PointCtx>(ctx_);
      switch (ctx->_0->tag) {
        case PointSize::tag::ProgramPointSize:
          break;
        default:
          throw "unsupported point size";
      }
      break;
    }
    case RasterContext::tag::LineCtx: {
      auto ctx = std::static_pointer_cast<data::LineCtx>(ctx_);
      glLineWidth(ctx->_0);
      break;
    }
    case RasterContext::tag::TriangleCtx: {
      auto ctx = std::static_pointer_cast<data::TriangleCtx>(ctx_);
      switch (ctx->_0->tag) {
        case CullMode::tag::CullNone:
          glDisable(GL_CULL_FACE);
          break;
        case CullMode::tag::CullFront: {
          auto f = std::static_pointer_cast<data::CullFront>(ctx->_0);
          glEnable(GL_CULL_FACE);
          glCullFace(GL_FRONT);
          glFrontFace(frontFace(f->_0));
          break;
        }
        case CullMode::tag::CullBack: {
          auto f = std::static_pointer_cast<data::CullBack>(ctx->_0);
          glEnable(GL_CULL_FACE);
          glCullFace(GL_BACK);
          glFrontFace(frontFace(f->_0));
          break;
        }
      }
      glDisable(GL_POLYGON_OFFSET_FILL);
      switch (ctx->_2->tag) {
        case PolygonOffset::tag::NoOffset:
          break;
        case PolygonOffset::tag::Offset: {
          auto o = std::static_pointer_cast<data::Offset>(ctx->_2);
          glPolygonOffset(o->_0,o->_1);
          glEnable(GL_POLYGON_OFFSET_FILL);
          break;
        }
      }
      break;
    }
  }
}

void setupAccumulationContext(std::shared_ptr<AccumulationContext> ctx_) {
  auto ctx = std::static_pointer_cast<data::AccumulationContext>(ctx_);
  bool noDepth = true;
  bool noStencil = true;
  bool noColor = true;
  for (auto i : ctx->accOperations) {
    switch (i->tag) {
      case FragmentOperation::tag::DepthOp: {
        auto o = std::static_pointer_cast<data::DepthOp>(i);
        noDepth = false;
        int df = comparisonFunction(o->_0);
        if (df == GL_ALWAYS && o->_1 == false) {
          glDisable(GL_DEPTH_TEST);
        } else {
          glEnable(GL_DEPTH_TEST);
          glDepthFunc(df);
          glDepthMask(o->_1);
        }
        break;
      }
      case FragmentOperation::tag::StencilOp: {
        auto o = std::static_pointer_cast<data::StencilOp>(i);
        noStencil = false;
        break;
      }
      case FragmentOperation::tag::ColorOp: {
        auto o = std::static_pointer_cast<data::ColorOp>(i);
        noColor = false;
        switch (o->_0->tag) {
          case Blending::tag::NoBlending:
            glDisable(GL_BLEND);
            break;
          case Blending::tag::BlendLogicOp:
            glDisable(GL_BLEND);
            break;
          case Blending::tag::Blend: {
            auto b = std::static_pointer_cast<data::Blend>(o->_0);
            glEnable(GL_BLEND);
            glBlendEquationSeparate(blendEquation(b->colorEqSrc),blendEquation(b->alphaEqSrc));
            glBlendColor(b->color.x,b->color.y,b->color.z,b->color.w);
            glBlendFuncSeparate(blendingFactor(b->colorFSrc),blendingFactor(b->colorFDst)
                               ,blendingFactor(b->alphaFSrc),blendingFactor(b->alphaFDst));
            break;
          }
        }
        bool maskR = true;
        bool maskG = true;
        bool maskB = true;
        bool maskA = true;
        switch (o->_1->tag) {
          case Value::tag::VBool: {
            auto v = std::static_pointer_cast<data::VBool>(o->_1);
            maskR = v->_0;
            break;
          }
          case Value::tag::VV2B: {
            auto v = std::static_pointer_cast<data::VV2B>(o->_1);
            maskR = v->_0.x;
            maskG = v->_0.y;
            break;
          }
          case Value::tag::VV3B: {
            auto v = std::static_pointer_cast<data::VV3B>(o->_1);
            maskR = v->_0.x;
            maskG = v->_0.y;
            maskB = v->_0.z;
            break;
          }
          case Value::tag::VV4B: {
            auto v = std::static_pointer_cast<data::VV4B>(o->_1);
            maskR = v->_0.x;
            maskG = v->_0.y;
            maskB = v->_0.z;
            maskA = v->_0.w;
            break;
          }
        }
        glColorMask(maskR,maskG,maskB,maskA);
        break;
      }
    }
  }
  if (noStencil) {
    glDisable(GL_STENCIL_TEST);
  }
  if (noDepth) {
    glDisable(GL_DEPTH_TEST);
  }
}

