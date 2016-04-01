package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
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
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.Int;
    uniforms.put(name, u);
  }

  public void setUniform(String name, Boolean v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.Bool;
    uniforms.put(name, u);
  }

  public void setUniform(String name, Float v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.Float;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V2I v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V2I;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V2B v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V2B;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V2F v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V2F;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V3I v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V3I;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V3B v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V3B;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V3F v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V3F;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V4I v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V4I;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V4B v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V4B;
    uniforms.put(name, u);
  }

  public void setUniform(String name, V4F v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.V4F;
    uniforms.put(name, u);
  }

  public void setUniform(String name, M22F v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.M22F;
    uniforms.put(name, u);
  }

  public void setUniform(String name, M33F v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.M33F;
    uniforms.put(name, u);
  }

  public void setUniform(String name, M44F v) throws Exception {
    UniformValue u = new UniformValue();
    u.tag = InputType.Tag.M44F;
    uniforms.put(name, u);
  }

}

