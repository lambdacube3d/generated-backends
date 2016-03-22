#include <iostream>
#include <OpenGLES/ES2/gl.h>

#include "LambdaCube.hpp"

// pipeline input
void Buffer::update(int i, std::vector<float> &v) {
}

int Buffer::add(std::vector<char> &v) {
  int i = data.size();
  data.push_back(v.data());
  size.push_back(v.size());
  byteSize.push_back(sizeof(char)*v.size());
  glType.push_back(GL_BYTE);
  return i;
}

int Buffer::add(std::vector<unsigned char> &v) {
  int i = data.size();
  data.push_back(v.data());
  size.push_back(v.size());
  byteSize.push_back(sizeof(unsigned char)*v.size());
  glType.push_back(GL_UNSIGNED_BYTE);
  return i;
}

int Buffer::add(std::vector<short> &v) {
  int i = data.size();
  data.push_back(v.data());
  size.push_back(v.size());
  byteSize.push_back(sizeof(short)*v.size());
  glType.push_back(GL_SHORT);
  return i;
}

int Buffer::add(std::vector<unsigned short> &v) {
  int i = data.size();
  data.push_back(v.data());
  size.push_back(v.size());
  byteSize.push_back(sizeof(unsigned short)*v.size());
  glType.push_back(GL_UNSIGNED_SHORT);
  return i;
}

int Buffer::add(std::vector<float> &v) {
  int i = data.size();
  data.push_back(v.data());
  size.push_back(v.size());
  byteSize.push_back(sizeof(float)*v.size());
  glType.push_back(GL_FLOAT);
  return i;
}

/*
  void BindBuffer( enum target, uint buffer );
    target:
      ARRAY_BUFFER
  void GenBuffers( sizei n, uint *buffers );
  void DeleteBuffers( sizei n, const uint *buffers );
  void BufferData( enum target, sizeiptr size, const void *data, enum usage );
    usage:
      STATIC_DRAW
      DYNAMIC_DRAW
      STREAM_DRAW
  void BufferSubData( enum target, intptr offset, sizeiptr size,const void*data);
*/
void Buffer::freeze() {
  unsigned int bufferSize = 0;
  for (auto i : byteSize) {
    offset.push_back(bufferSize);
    bufferSize += i;
  }
  unsigned int bo;
  glGenBuffers(1,&bo);
  glBindBuffer(GL_ARRAY_BUFFER,bo);
  glBufferData(GL_ARRAY_BUFFER,bufferSize,nullptr,GL_STATIC_DRAW);
  unsigned int offset_ = 0;
  for (int i = 0; i < data.size(); i++) {
    glBufferSubData(GL_ARRAY_BUFFER, offset_, byteSize[i], data[i]);
    offset_ += byteSize[i];
  }
  bufferObject = bo;
}

// Stream
Stream::Stream(Float& v) {
  type = Type::FLOAT;
  isArray = false;
  _float = v;
  glSize = 1;
}
Stream::Stream(V2F& v) {
  type = Type::FLOAT_VEC2;
  isArray = false;
  _v2f = v;
  glSize = 2;
}
Stream::Stream(V3F& v) {
  type = Type::FLOAT_VEC3;
  isArray = false;
  _v3f = v;
  glSize = 3;
}
Stream::Stream(V4F& v) {
  type = Type::FLOAT_VEC4;
  isArray = false;
  _v4f = v;
  glSize = 4;
}
Stream::Stream(M22F& v) {
  type = Type::FLOAT_MAT2;
  isArray = false;
  _m22f = v;
  glSize = 4;
}
Stream::Stream(M33F& v) {
  type = Type::FLOAT_MAT3;
  isArray = false;
  _m33f = v;
  glSize = 9;
}
Stream::Stream(M44F& v) {
  type = Type::FLOAT_MAT4;
  isArray = false;
  _m44f = v;
  glSize = 16;
}
Stream::Stream(std::shared_ptr<Buffer> b, int i, Type t) {
  type = t;
  buffer = b;
  index = i;
  isArray = true;
  switch (t) {
    case Type::FLOAT:       glSize = 1; break;
    case Type::FLOAT_VEC2:  glSize = 2; break;
    case Type::FLOAT_VEC3:  glSize = 3; break;
    case Type::FLOAT_VEC4:  glSize = 4; break;
    case Type::FLOAT_MAT2:  glSize = 2; break;
    case Type::FLOAT_MAT3:  glSize = 9; break;
    case Type::FLOAT_MAT4:  glSize = 16;break;
  }
}

// StreamMap
void StreamMap::add(std::string name, Float& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, V2F& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, V3F& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, V4F& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, M22F& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, M33F& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, M44F& v) {
  map[name] = std::shared_ptr<Stream>(new Stream(v));
}
void StreamMap::add(std::string name, Type t, std::shared_ptr<Buffer> b, int index) {
  map[name] = std::shared_ptr<Stream>(new Stream(b,index,t));
}
bool StreamMap::validate() {
  return true;
}

// Object
Object::~Object() {
}
void Object::enable(bool visible) {
  enabled = visible;
}
void Object::setOrder(int o) {
  order = o;
}
void Object::setUniform(std::string name, Int& v) {
  uniforms[name] = {.tag = InputType::tag::Int, ._int = v};
}
void Object::setUniform(std::string name, Word& v) {
  uniforms[name] = {.tag = InputType::tag::Word, ._word = v};
}
void Object::setUniform(std::string name, Float& v) {
  uniforms[name] = {.tag = InputType::tag::Float, ._float = v};
}
void Object::setUniform(std::string name, Bool& v) {
  uniforms[name] = {.tag = InputType::tag::Bool, ._bool = v};
}
void Object::setUniform(std::string name, V2I& v) {
  uniforms[name] = {.tag = InputType::tag::V2I, ._v2i = v};
}
void Object::setUniform(std::string name, V2U& v) {
  uniforms[name] = {.tag = InputType::tag::V2U, ._v2u = v};
}
void Object::setUniform(std::string name, V2F& v) {
  uniforms[name] = {.tag = InputType::tag::V2F, ._v2f = v};
}
void Object::setUniform(std::string name, V2B& v) {
  uniforms[name] = {.tag = InputType::tag::V2B, ._v2b = v};
}
void Object::setUniform(std::string name, V3I& v) {
  uniforms[name] = {.tag = InputType::tag::V3I, ._v3i = v};
}
void Object::setUniform(std::string name, V3U& v) {
  uniforms[name] = {.tag = InputType::tag::V3U, ._v3u = v};
}
void Object::setUniform(std::string name, V3F& v) {
  uniforms[name] = {.tag = InputType::tag::V3F, ._v3f = v};
}
void Object::setUniform(std::string name, V3B& v) {
  uniforms[name] = {.tag = InputType::tag::V3B, ._v3b = v};
}
void Object::setUniform(std::string name, V4I& v) {
  uniforms[name] = {.tag = InputType::tag::V4I, ._v4i = v};
}
void Object::setUniform(std::string name, V4U& v) {
  uniforms[name] = {.tag = InputType::tag::V4U, ._v4u = v};
}
void Object::setUniform(std::string name, V4F& v) {
  uniforms[name] = {.tag = InputType::tag::V4F, ._v4f = v};
}
void Object::setUniform(std::string name, V4B& v) {
  uniforms[name] = {.tag = InputType::tag::V4B, ._v4b = v};
}
void Object::setUniform(std::string name, M22F& v) {
  uniforms[name] = {.tag = InputType::tag::M22F, ._m22f = v};
}
void Object::setUniform(std::string name, M33F& v) {
  uniforms[name] = {.tag = InputType::tag::M33F, ._m33f = v};
}
void Object::setUniform(std::string name, M44F& v) {
  uniforms[name] = {.tag = InputType::tag::M44F, ._m44f = v};
}

// PipelineInput
std::shared_ptr<Object> PipelineInput::createObject(String slotName, Primitive prim, std::shared_ptr<StreamMap> attributes, std::vector<String> objectUniforms) {
  std::shared_ptr<Object> o(new Object());
  o->enabled = true;
  o->order = 0;
  o->glMode = primitiveMode(prim);
  int count = 0;
  for (auto i : attributes->map) {
    if (i.second->isArray) {
      count = i.second->buffer->size[i.second->index] / i.second->glSize;
      break;
    }
  }
  o->glCount = count;
  o->streams = attributes;
  if (objectMap.count(slotName)>0) {
    objectMap[slotName]->push_back(o);
  } else {
    objectMap[slotName] = std::shared_ptr<std::vector<std::shared_ptr<Object>>>(new std::vector<std::shared_ptr<Object>>({o}));
  }
  return o;
}
std::shared_ptr<Object> PipelineInput::createObject(String slotName, Primitive prim, StreamMap& attributes, Buffer& indexBuffer, int bufferIndex, std::vector<String> objectUniforms) {
  // TODO
  return std::shared_ptr<Object>();
}
void PipelineInput::sortSlotObjects() {
}
void PipelineInput::setScreenSize(int w,int h) {
  screenWidth = w;
  screenHeight = h;
}

void PipelineInput::setUniform(std::string name, Int& v) {
  uniforms[name] = {.tag = InputType::tag::Int, ._int = v};
}
void PipelineInput::setUniform(std::string name, Word& v) {
  uniforms[name] = {.tag = InputType::tag::Word, ._word = v};
}
void PipelineInput::setUniform(std::string name, Float& v) {
  uniforms[name] = {.tag = InputType::tag::Float, ._float = v};
}
void PipelineInput::setUniform(std::string name, Bool& v) {
  uniforms[name] = {.tag = InputType::tag::Bool, ._bool = v};
}
void PipelineInput::setUniform(std::string name, V2I& v) {
  uniforms[name] = {.tag = InputType::tag::V2I, ._v2i = v};
}
void PipelineInput::setUniform(std::string name, V2U& v) {
  uniforms[name] = {.tag = InputType::tag::V2U, ._v2u = v};
}
void PipelineInput::setUniform(std::string name, V2F& v) {
  uniforms[name] = {.tag = InputType::tag::V2F, ._v2f = v};
}
void PipelineInput::setUniform(std::string name, V2B& v) {
  uniforms[name] = {.tag = InputType::tag::V2B, ._v2b = v};
}
void PipelineInput::setUniform(std::string name, V3I& v) {
  uniforms[name] = {.tag = InputType::tag::V3I, ._v3i = v};
}
void PipelineInput::setUniform(std::string name, V3U& v) {
  uniforms[name] = {.tag = InputType::tag::V3U, ._v3u = v};
}
void PipelineInput::setUniform(std::string name, V3F& v) {
  uniforms[name] = {.tag = InputType::tag::V3F, ._v3f = v};
}
void PipelineInput::setUniform(std::string name, V3B& v) {
  uniforms[name] = {.tag = InputType::tag::V3B, ._v3b = v};
}
void PipelineInput::setUniform(std::string name, V4I& v) {
  uniforms[name] = {.tag = InputType::tag::V4I, ._v4i = v};
}
void PipelineInput::setUniform(std::string name, V4U& v) {
  uniforms[name] = {.tag = InputType::tag::V4U, ._v4u = v};
}
void PipelineInput::setUniform(std::string name, V4F& v) {
  uniforms[name] = {.tag = InputType::tag::V4F, ._v4f = v};
}
void PipelineInput::setUniform(std::string name, V4B& v) {
  uniforms[name] = {.tag = InputType::tag::V4B, ._v4b = v};
}
void PipelineInput::setUniform(std::string name, M22F& v) {
  uniforms[name] = {.tag = InputType::tag::M22F, ._m22f = v};
}
void PipelineInput::setUniform(std::string name, M33F& v) {
  uniforms[name] = {.tag = InputType::tag::M33F, ._m33f = v};
}
void PipelineInput::setUniform(std::string name, M44F& v) {
  uniforms[name] = {.tag = InputType::tag::M44F, ._m44f = v};
}
