#include <iostream>
#include <OpenGLES/ES2/gl.h>

#include "LambdaCube.hpp"

// enum functions
enum Type inputType(std::shared_ptr<InputType> t) {
  switch (t->tag) {
    case InputType::tag::Float: return Type::FLOAT;
    case InputType::tag::V2F:   return Type::FLOAT_VEC2;
    case InputType::tag::V3F:   return Type::FLOAT_VEC3;
    case InputType::tag::V4F:   return Type::FLOAT_VEC4;
    case InputType::tag::M22F:  return Type::FLOAT_MAT2;
    case InputType::tag::M33F:  return Type::FLOAT_MAT3;
    case InputType::tag::M44F:  return Type::FLOAT_MAT4;
  }
  throw "illegal input type";
}

int primitiveMode(Primitive p) {
  switch (p) {
    case Primitive::TriangleStrip:  return GL_TRIANGLE_STRIP;
    case Primitive::TriangleList:   return GL_TRIANGLES;
    case Primitive::TriangleFan:    return GL_TRIANGLE_FAN;
    case Primitive::LineStrip:      return GL_LINE_STRIP;
    case Primitive::LineList:       return GL_LINES;
    case Primitive::LineLoop:       return GL_LINE_LOOP;
    case Primitive::PointList:      return GL_POINTS;
  }
}

int blendingFactor(std::shared_ptr<::BlendingFactor> bf) {
  switch (bf->tag) {
    case BlendingFactor::tag::ConstantAlpha:          return GL_CONSTANT_ALPHA;
    case BlendingFactor::tag::ConstantColor:          return GL_CONSTANT_COLOR;
    case BlendingFactor::tag::DstAlpha:               return GL_DST_ALPHA;
    case BlendingFactor::tag::DstColor:               return GL_DST_COLOR;
    case BlendingFactor::tag::One:                    return GL_ONE;
    case BlendingFactor::tag::OneMinusConstantAlpha:  return GL_ONE_MINUS_CONSTANT_ALPHA;
    case BlendingFactor::tag::OneMinusConstantColor:  return GL_ONE_MINUS_CONSTANT_COLOR;
    case BlendingFactor::tag::OneMinusDstAlpha:       return GL_ONE_MINUS_DST_ALPHA;
    case BlendingFactor::tag::OneMinusDstColor:       return GL_ONE_MINUS_DST_COLOR;
    case BlendingFactor::tag::OneMinusSrcAlpha:       return GL_ONE_MINUS_SRC_ALPHA;
    case BlendingFactor::tag::OneMinusSrcColor:       return GL_ONE_MINUS_SRC_COLOR;
    case BlendingFactor::tag::SrcAlpha:               return GL_SRC_ALPHA;
    case BlendingFactor::tag::SrcAlphaSaturate:       return GL_SRC_ALPHA_SATURATE;
    case BlendingFactor::tag::SrcColor:               return GL_SRC_COLOR;
    case BlendingFactor::tag::Zero:                   return GL_ZERO;
  }
  throw "illegal blending factor";
}

int blendEquation(std::shared_ptr<::BlendEquation> be) {
  switch (be->tag) {
    case BlendEquation::tag::FuncAdd:             return GL_FUNC_ADD;
    case BlendEquation::tag::FuncReverseSubtract: return GL_FUNC_REVERSE_SUBTRACT;
    case BlendEquation::tag::FuncSubtract:        return GL_FUNC_SUBTRACT;
  }
  throw "illegal blend equation";
}

int comparisonFunction(std::shared_ptr<::ComparisonFunction> cf) {
  switch (cf->tag) {
    case ComparisonFunction::tag::Always:   return GL_ALWAYS;
    case ComparisonFunction::tag::Equal:    return GL_EQUAL;
    case ComparisonFunction::tag::Gequal:   return GL_GEQUAL;
    case ComparisonFunction::tag::Greater:  return GL_GREATER;
    case ComparisonFunction::tag::Lequal:   return GL_LEQUAL;
    case ComparisonFunction::tag::Less:     return GL_LESS;
    case ComparisonFunction::tag::Never:    return GL_NEVER;
    case ComparisonFunction::tag::Notequal: return GL_NOTEQUAL;
  }
  throw "illegal comparison function";
}

int frontFace(std::shared_ptr<::FrontFace> ff) {
  switch (ff->tag) {
    case FrontFace::tag::CCW: return GL_CCW;
    case FrontFace::tag::CW:  return GL_CW;
  }
  throw "illegal front face value";
}

/*
      ALPHA
      LUMINANCE
      LUMINANCE_ALPHA
      RGB
      RGBA
*/
int textureDataTypeToGLType(std::shared_ptr<ImageSemantic> s_, std::shared_ptr<TextureDataType> d_) {
  // TODO
  switch (s_->tag) {
    case ImageSemantic::tag::Color:
      return GL_RGBA;
      break;
    case ImageSemantic::tag::Depth:
      return GL_DEPTH_COMPONENT;
      break;
    case ImageSemantic::tag::Stencil:
      break;
  }
  throw "FIXME: This texture format is not yet supported";
}

int textureDataTypeToGLArityType(std::shared_ptr<ImageSemantic> s_, std::shared_ptr<TextureDataType> d_) {
  // TODO
  switch (s_->tag) {
    case ImageSemantic::tag::Color:
      return GL_RGBA;
      break;
    case ImageSemantic::tag::Depth:
      return GL_DEPTH_COMPONENT;
      break;
    case ImageSemantic::tag::Stencil:
      break;
  }
  throw "FIXME: This texture format is not yet supported";
}

int edgeMode(std::shared_ptr<::EdgeMode> e) {
  switch (e->tag) {
    case EdgeMode::tag::ClampToEdge:    return GL_CLAMP_TO_EDGE;
    case EdgeMode::tag::Repeat:         return GL_REPEAT;
    case EdgeMode::tag::MirroredRepeat: return GL_MIRRORED_REPEAT;
    default:
      throw "unsupported edge mode";
  }
}

int filterMode(std::shared_ptr<::Filter> f) {
  switch (f->tag) {
    case Filter::tag::Nearest:              return GL_NEAREST;
    case Filter::tag::Linear:               return GL_LINEAR;
    case Filter::tag::NearestMipmapNearest: return GL_NEAREST_MIPMAP_NEAREST;
    case Filter::tag::NearestMipmapLinear:  return GL_NEAREST_MIPMAP_LINEAR;
    case Filter::tag::LinearMipmapNearest:  return GL_LINEAR_MIPMAP_NEAREST;
    case Filter::tag::LinearMipmapLinear:   return GL_LINEAR_MIPMAP_LINEAR;
    default:
      throw "unsupported filter mode";
  }
}

