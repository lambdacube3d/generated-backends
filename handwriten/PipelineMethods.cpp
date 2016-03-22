#include <iostream>
#include <OpenGLES/ES2/gl.h>

#include "LambdaCube.hpp"

// pipeline class
unsigned int GLES20Pipeline::createRenderTarget(std::shared_ptr<RenderTarget> t_) {
  auto t = std::static_pointer_cast<data::RenderTarget>(t_);
  // does this target have texture attachments?
  int textureCount = 0;
  for (auto i_ : t->renderTargets) {
    auto i = std::static_pointer_cast<data::TargetItem>(i_);
    if (i->targetRef.valid && i->targetRef.data->tag == ImageRef::tag::TextureImage)
      textureCount++;
  }
  if (textureCount == 0)
    return 0;

  // has textures attachment
  unsigned int fb;
  glGenFramebuffers(1,&fb);
  glBindFramebuffer(GL_FRAMEBUFFER, fb);
  /*
  void FramebufferTexture2D( enum target, enum attachment,enum textarget, uint texture, int level );
    attachment:
      COLOR_ATTACHMENT0
      DEPTH_ATTACHMENT
      STENCIL_ATTACHMENT
  */
  int attachment,textarget,level;
  unsigned int texture;
  for (auto i_ : t->renderTargets) {
    auto i = std::static_pointer_cast<data::TargetItem>(i_);
    switch (i->targetSemantic->tag) {
      case ImageSemantic::tag::Color:
        attachment = GL_COLOR_ATTACHMENT0;
        break;
      case ImageSemantic::tag::Depth:
        attachment = GL_DEPTH_ATTACHMENT;
        break;
      case ImageSemantic::tag::Stencil:
        attachment = GL_STENCIL_ATTACHMENT;
        break;
    }
/*
    GL_TEXTURE_2D
    GL_TEXTURE_CUBE_MAP_POSITIVE_X
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
*/
    if (i->targetRef.valid) {
      switch (i->targetRef.data->tag) {
        case ImageRef::tag::TextureImage: {
          auto ti = std::static_pointer_cast<data::TextureImage>(i->targetRef.data);
          texture = textures[ti->_0].texture;
          textarget = GL_TEXTURE_2D; // TODO
          level = ti->_1;
          break;
        }
        case ImageRef::tag::Framebuffer:
          texture = 0;
          textarget = GL_TEXTURE_2D;
          level = 0;
          break;
      }
    } else {
      texture = 0;
      textarget = GL_TEXTURE_2D;
      level = 0;
    }
    glFramebufferTexture2D(GL_FRAMEBUFFER,attachment,textarget,texture,level);
  }
  return fb;
}

GLES20Pipeline::GLES20Pipeline(std::shared_ptr<Pipeline> ppl_) {
  screenTarget = 0;
  hasCurrentProgram = false;
  auto ppl = std::static_pointer_cast<data::Pipeline>(ppl_);
  pipeline = ppl;
  // check backend compatibility
  if (ppl->backend->tag != Backend::tag::WebGL1) {
    throw "unsupported backend";
  }
  // allocate all resources
  //  textures
  for (auto i : ppl->textures) {
    textures.push_back(createTexture(i));
  }
  //  targets
  for (auto i : ppl->targets) {
    targets.push_back(createRenderTarget(i));
  }
  //  programs
  for (auto i : ppl->programs) {
    programs.push_back(createProgram(i));
  }
  //  stream data
  for (auto i : ppl->streams) {
    streamData.push_back(createStreamData(i));
  }
  glReleaseShaderCompiler();
}

GLES20Pipeline::~GLES20Pipeline() {
  // release resources
  // textures
  for (auto i : textures) {
    glDeleteTextures(1,&i.texture);
  }
  // targets
  for (auto i : targets) {
    glDeleteFramebuffers(1,&i);
  }
  // programs
  for (auto i : programs) {
    glDeleteProgram(i->program);
    glDeleteShader(i->vertexShader);
    glDeleteShader(i->fragmentShader);
  }
}

void GLES20Pipeline::setPipelineInput(std::shared_ptr<PipelineInput> i) {
  input = i;
}

void GLES20Pipeline::render() {
  //std::cout << "render\n";
  for (auto i : pipeline->commands) {
    switch (i->tag) {
      case Command::tag::SetRasterContext: {
        auto cmd = std::static_pointer_cast<data::SetRasterContext>(i);
        setupRasterContext(cmd->_0);
        break;
      }
      case Command::tag::SetAccumulationContext: {
        auto cmd = std::static_pointer_cast<data::SetAccumulationContext>(i);
        setupAccumulationContext(cmd->_0);
        break;
      }
      case Command::tag::SetTexture: {
        auto cmd = std::static_pointer_cast<data::SetTexture>(i);
        glActiveTexture(GL_TEXTURE0 + cmd->_0);
        glBindTexture(textures[cmd->_1].target,textures[cmd->_1].texture);
        break;
      }
      case Command::tag::SetProgram: {
        auto cmd = std::static_pointer_cast<data::SetProgram>(i);
        hasCurrentProgram = true;
        currentProgram = cmd->_0;
        glUseProgram(programs[currentProgram]->program);
        break;
      }
      case Command::tag::SetRenderTarget: {
        auto cmd = std::static_pointer_cast<data::SetRenderTarget>(i);
        unsigned int t = targets[cmd->_0];
        glBindFramebuffer(GL_FRAMEBUFFER, (t==0)?screenTarget:t);
        if (input) {
          glViewport(0,0,input->screenWidth,input->screenHeight);
        }
        break;
      }
      case Command::tag::ClearRenderTarget: {
        auto cmd = std::static_pointer_cast<data::ClearRenderTarget>(i);
        unsigned int mask = 0;
        for (auto a : cmd->_0) {
          auto image = std::static_pointer_cast<data::ClearImage>(a);
          switch (image->imageSemantic->tag) {
            case ImageSemantic::tag::Depth: {
              auto v = std::static_pointer_cast<data::VFloat>(image->clearValue);
              glDepthMask(true);
              glClearDepthf(v->_0);
              mask |= GL_DEPTH_BUFFER_BIT;
              break;
            }
            case ImageSemantic::tag::Stencil: {
              auto v = std::static_pointer_cast<data::VWord>(image->clearValue);
              glClearStencil(v->_0);
              mask |= GL_STENCIL_BUFFER_BIT;
              break;
            }
            case ImageSemantic::tag::Color:
              switch (image->clearValue->tag) {
                case Value::tag::VFloat: {
                  auto v = std::static_pointer_cast<data::VFloat>(image->clearValue);
                  glClearColor(v->_0,0.0,0.0,1.0);
                  break;
                }
                case Value::tag::VV2F: {
                  auto v = std::static_pointer_cast<data::VV2F>(image->clearValue);
                  glClearColor(v->_0.x,v->_0.y,0.0,1.0);
                  break;
                }
                case Value::tag::VV3F: {
                  auto v = std::static_pointer_cast<data::VV3F>(image->clearValue);
                  glClearColor(v->_0.x,v->_0.y,v->_0.z,1.0);
                  break;
                }
                case Value::tag::VV4F: {
                  auto v = std::static_pointer_cast<data::VV4F>(image->clearValue);
                  glClearColor(v->_0.x,v->_0.y,v->_0.z,v->_0.w);
                  break;
                }
                default:
                  glClearColor(0.0,0.0,0.0,1.0);
              }
              glColorMask(true,true,true,true);
              mask |= GL_COLOR_BUFFER_BIT;
              break;
          }
        }
        glClear(mask);
        break;
      }
      case Command::tag::SetSamplerUniform: if (hasCurrentProgram) {
        auto cmd = std::static_pointer_cast<data::SetSamplerUniform>(i);
        int sampler = programs[currentProgram]->programInTextures[cmd->_0];
        glUniform1i(sampler,cmd->_1);
        break;
      }
      case Command::tag::RenderSlot: if (input && pipeline && hasCurrentProgram) {
        auto cmd = std::static_pointer_cast<data::RenderSlot>(i);
        auto slot = std::static_pointer_cast<data::Slot>(pipeline->slots[cmd->_0]);
        if (input->objectMap.count(slot->slotName)<=0)
          break;
        for (auto o: *input->objectMap[slot->slotName]) {
          if (!o->enabled) continue;
          // setup uniforms
          for (auto u: programs[currentProgram]->programUniforms) {
            if (o->uniforms.count(u.first)>0) {
              setUniformValue(u.second,o->uniforms[u.first]);
            } else {
              setUniformValue(u.second,input->uniforms[u.first]);
            }
          }
          // setup streams
          for (auto s: programs[currentProgram]->programStreams) {
            setStream(s.second.index,*o->streams->map[s.second.name]);
          }
          // draw call
          // TODO: support index buffers
          glDrawArrays(o->glMode, 0, o->glCount);
        }
        break;
      }
      case Command::tag::RenderStream: if (input && pipeline && hasCurrentProgram) {
        auto cmd = std::static_pointer_cast<data::RenderStream>(i);
        auto data = streamData[cmd->_0];
        // setup streams
        for (auto s: programs[currentProgram]->programStreams) {
          setStream(s.second.index,*data->streams.map[s.second.name]);
        }
        // draw call
        // TODO: support index buffers
        glDrawArrays(data->glMode, 0, data->glCount);
        break;
      }

      // unused commands
      case Command::tag::GenerateMipMap:
      case Command::tag::SetSampler:
      case Command::tag::SaveImage:
      case Command::tag::LoadImage:
        break;
    }
  }
}
