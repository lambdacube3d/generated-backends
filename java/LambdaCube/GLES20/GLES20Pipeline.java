package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class GLES20Pipeline {
  protected PipelineInput input;
  protected Pipeline.Pipeline_ pipeline;
  protected ArrayList<Texture> textures;
  protected ArrayList<Integer> targets;
  protected ArrayList<GLProgram> programs;
  protected ArrayList<GLStreamData> streamData;
  protected Integer currentProgram;
  protected Boolean hasCurrentProgram;
  protected Integer createRenderTarget(RenderTarget t_) throws Exception {
    RenderTarget.RenderTarget_ t = (RenderTarget.RenderTarget_)t_;
    Integer textureCount = 0;
    for (TargetItem i_ : t.renderTargets) {
      TargetItem.TargetItem_ i = (TargetItem.TargetItem_)i_;
      if (i.targetRef.valid && i.targetRef.data.tag == ImageRef.Tag.TextureImage) {
        textureCount++;
      }
    }
    if (textureCount == 0) {
      return 0;
    }
    Integer fb;
    { int[] glObj = new int[1]; GLES20.glGenFramebuffers(1, glObj, 0); fb = glObj[0]; }
    GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, fb);
    Integer attachment = 0;
    Integer textarget = 0;
    Integer level = 0;
    Integer texture = 0;
    for (TargetItem i_ : t.renderTargets) {
      TargetItem.TargetItem_ i = (TargetItem.TargetItem_)i_;
      switch (i.targetSemantic.tag) {
        case Color: attachment = GLES20.GL_COLOR_ATTACHMENT0; break;
        case Depth: attachment = GLES20.GL_DEPTH_ATTACHMENT; break;
        case Stencil: attachment = GLES20.GL_STENCIL_ATTACHMENT; break;
      }
      if (i.targetRef.valid) {
        switch (i.targetRef.data.tag) {
          case TextureImage: {
            ImageRef.TextureImage_ ti = (ImageRef.TextureImage_)i.targetRef.data;
            texture = textures.get(ti._0).texture;
            textarget = GLES20.GL_TEXTURE_2D;
            level = ti._1;
            break;
          }
          case Framebuffer: {
            texture = 0;
            textarget = GLES20.GL_TEXTURE_2D;
            level = 0;
            break;
          }
        }
      } else {
        texture = 0;
        textarget = GLES20.GL_TEXTURE_2D;
        level = 0;
      }
      GLES20.glFramebufferTexture2D(GLES20.GL_FRAMEBUFFER, attachment, textarget, texture, level);
    }
    return fb;
  }

  public Integer screenTarget;
  public GLES20Pipeline(Pipeline ppl_) throws Exception {
    
    textures = new ArrayList<Texture>();
    targets = new ArrayList<Integer>();
    programs = new ArrayList<GLProgram>();
    streamData = new ArrayList<GLStreamData>();

    screenTarget = 0;
    hasCurrentProgram = false;
    Pipeline.Pipeline_ ppl = (Pipeline.Pipeline_)ppl_;
    pipeline = ppl;
    if (ppl.backend.tag != Backend.Tag.WebGL1) {
      throw new Exception("unsupported backend");
    }
    for (TextureDescriptor i : ppl.textures) {
      textures.add(Util.createTexture(i));
    }
    for (RenderTarget i : ppl.targets) {
      targets.add(createRenderTarget(i));
    }
    for (Program i : ppl.programs) {
      programs.add(Util.createProgram(i));
    }
    for (StreamData i : ppl.streams) {
      streamData.add(Util.createStreamData(i));
    }
    GLES20.glReleaseShaderCompiler();
  }

  protected void finalize() {
    for (Texture i : textures) {
      { int[] glObj = new int[1]; glObj[0] = i.texture; GLES20.glDeleteTextures(1, glObj, 0);}
    }
    for (Integer i : targets) {
      { int[] glObj = new int[1]; glObj[0] = i; GLES20.glDeleteFramebuffers(1, glObj, 0);}
    }
    for (GLProgram i : programs) {
      GLES20.glDeleteProgram(i.program);
      GLES20.glDeleteShader(i.vertexShader);
      GLES20.glDeleteShader(i.fragmentShader);
    }
  }

  public void setPipelineInput(PipelineInput i) throws Exception {
    input = i;
  }

  public void render() throws Exception {
    for (Command i : pipeline.commands) {
      switch (i.tag) {
        case SetRasterContext: {
          Command.SetRasterContext_ cmd = (Command.SetRasterContext_)i;
          Util.setupRasterContext(cmd._0);
          break;
        }
        case SetAccumulationContext: {
          Command.SetAccumulationContext_ cmd = (Command.SetAccumulationContext_)i;
          Util.setupAccumulationContext(cmd._0);
          break;
        }
        case SetTexture: {
          Command.SetTexture_ cmd = (Command.SetTexture_)i;
          GLES20.glActiveTexture(GLES20.GL_TEXTURE0 + cmd._0);
          GLES20.glBindTexture(textures.get(cmd._1).target, textures.get(cmd._1).texture);
          break;
        }
        case SetProgram: {
          Command.SetProgram_ cmd = (Command.SetProgram_)i;
          hasCurrentProgram = true;
          currentProgram = cmd._0;
          GLES20.glUseProgram(programs.get(currentProgram).program);
          break;
        }
        case SetRenderTarget: {
          Command.SetRenderTarget_ cmd = (Command.SetRenderTarget_)i;
          Integer t = targets.get(cmd._0);
          GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, t == 0?screenTarget:t);
          if (input!= null) {
            GLES20.glViewport(0, 0, input.screenWidth, input.screenHeight);
          }
          break;
        }
        case ClearRenderTarget: {
          Command.ClearRenderTarget_ cmd = (Command.ClearRenderTarget_)i;
          Integer mask = 0;
          for (ClearImage a : cmd._0) {
            ClearImage.ClearImage_ image = (ClearImage.ClearImage_)a;
            switch (image.imageSemantic.tag) {
              case Depth: {
                Value.VFloat_ v = (Value.VFloat_)image.clearValue;
                GLES20.glDepthMask(true);
                GLES20.glClearDepthf(v._0);
                mask |= GLES20.GL_DEPTH_BUFFER_BIT;
                break;
              }
              case Stencil: {
                Value.VWord_ v = (Value.VWord_)image.clearValue;
                GLES20.glClearStencil(v._0);
                mask |= GLES20.GL_STENCIL_BUFFER_BIT;
                break;
              }
              case Color: {
                switch (image.clearValue.tag) {
                  case VFloat: {
                    Value.VFloat_ v = (Value.VFloat_)image.clearValue;
                    GLES20.glClearColor(v._0, 0.0f, 0.0f, 1.0f);
                    break;
                  }
                  case VV2F: {
                    Value.VV2F_ v = (Value.VV2F_)image.clearValue;
                    GLES20.glClearColor(v._0.x, v._0.y, 0.0f, 1.0f);
                    break;
                  }
                  case VV3F: {
                    Value.VV3F_ v = (Value.VV3F_)image.clearValue;
                    GLES20.glClearColor(v._0.x, v._0.y, v._0.z, 1.0f);
                    break;
                  }
                  case VV4F: {
                    Value.VV4F_ v = (Value.VV4F_)image.clearValue;
                    GLES20.glClearColor(v._0.x, v._0.y, v._0.z, v._0.w);
                    break;
                  }
                  default:
                    GLES20.glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

                }
                GLES20.glColorMask(true, true, true, true);
                mask |= GLES20.GL_COLOR_BUFFER_BIT;
                break;
              }
            }
          }
          GLES20.glClear(mask);
          break;
        }
        case SetSamplerUniform: {
          if (hasCurrentProgram) {
            Command.SetSamplerUniform_ cmd = (Command.SetSamplerUniform_)i;
            Integer sampler = programs.get(currentProgram).programInTextures.get(cmd._0);
            GLES20.glUniform1i(sampler, cmd._1);
          }
          break;
        }
        case RenderSlot: {
          if (input!= null && pipeline!= null && hasCurrentProgram) {
            Command.RenderSlot_ cmd = (Command.RenderSlot_)i;
            Slot.Slot_ slot = (Slot.Slot_)pipeline.slots.get(cmd._0);
            if (!input.objectMap.containsKey(slot.slotName)) {
              break;
            }
            for (Object o : input.objectMap.get(slot.slotName)) {
              if (!o.enabled) {
                continue;
              }
              for (Map.Entry<String,Integer> u : programs.get(currentProgram).programUniforms.entrySet()) {
                if (o.uniforms.containsKey(u.getKey())) {
                  Util.setUniformValue(u.getValue(), o.uniforms.get(u.getKey()));
                } else {
                  Util.setUniformValue(u.getValue(), input.uniforms.get(u.getKey()));
                }
              }
              for (Map.Entry<String,StreamInfo> s : programs.get(currentProgram).programStreams.entrySet()) {
                Util.setStream(s.getValue().index, o.streams.map.get(s.getValue().name));
              }
              GLES20.glDrawArrays(o.glMode, 0, o.glCount);
            }
          }
          break;
        }
        case RenderStream: {
          if (input!= null && pipeline!= null && hasCurrentProgram) {
            Command.RenderStream_ cmd = (Command.RenderStream_)i;
            GLStreamData data = streamData.get(cmd._0);
            for (Map.Entry<String,Integer> u : programs.get(currentProgram).programUniforms.entrySet()) {
              Util.setUniformValue(u.getValue(), input.uniforms.get(u.getKey()));
            }
            for (Map.Entry<String,StreamInfo> s : programs.get(currentProgram).programStreams.entrySet()) {
              Util.setStream(s.getValue().index, data.streams.map.get(s.getValue().name));
            }
            GLES20.glDrawArrays(data.glMode, 0, data.glCount);
          }
          break;
        }
      }
    }
  }

}

