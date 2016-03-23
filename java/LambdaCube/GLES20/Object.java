package LambdaCube.GLES20;

import android.opengl.GLES20;
import LambdaCube.IR.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Object {
  public Boolean enabled;
  public Integer order, glMode, glCount;
  public HashMap<String,UniformValue> uniforms;
  public StreamMap streams;
  protected void finalize() {
  }

  public void enable(Boolean visible) throws Exception {
    enabled = visible;
  }

  public void setOrder(Integer o) throws Exception {
    order = o;
  }

  public void setUniform(String name, Integer v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.Int, ._int = v});
  }

  public void setUniform(String name, Integer v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.Word, ._word = v});
  }

  public void setUniform(String name, Float v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.Float, ._float = v});
  }

  public void setUniform(String name, Boolean v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.Bool, ._bool = v});
  }

  public void setUniform(String name, V2I v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V2I, ._v2i = v});
  }

  public void setUniform(String name, V2U v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V2U, ._v2u = v});
  }

  public void setUniform(String name, V2F v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V2F, ._v2f = v});
  }

  public void setUniform(String name, V2B v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V2B, ._v2b = v});
  }

  public void setUniform(String name, V3I v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V3I, ._v3i = v});
  }

  public void setUniform(String name, V3U v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V3U, ._v3u = v});
  }

  public void setUniform(String name, V3F v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V3F, ._v3f = v});
  }

  public void setUniform(String name, V3B v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V3B, ._v3b = v});
  }

  public void setUniform(String name, V4I v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V4I, ._v4i = v});
  }

  public void setUniform(String name, V4U v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V4U, ._v4u = v});
  }

  public void setUniform(String name, V4F v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V4F, ._v4f = v});
  }

  public void setUniform(String name, V4B v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.V4B, ._v4b = v});
  }

  public void setUniform(String name, M22F v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.M22F, ._m22f = v});
  }

  public void setUniform(String name, M33F v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.M33F, ._m33f = v});
  }

  public void setUniform(String name, M44F v) throws Exception {
    uniforms.put(name, {.tag = InputType.Tag.M44F, ._m44f = v});
  }

}

