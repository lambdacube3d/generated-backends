package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class GLObject {
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
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.Int;
      u._int = new int[1];
      u._int[0] = v;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, Boolean v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v?1:0;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.Bool;
      u._int = new int[1];
      u._int[0] = v?1:0;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, Float v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.Float;
      u._float = new float[1];
      u._float[0] = v;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V2I v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v.x;
      u._int[1] = v.y;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V2I;
      u._int = new int[2];
      u._int[0] = v.x;
      u._int[1] = v.y;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V2B v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v.x?1:0;
      u._int[1] = v.y?1:0;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V2B;
      u._int = new int[2];
      u._int[0] = v.x?1:0;
      u._int[1] = v.y?1:0;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V2F v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v.x;
      u._float[1] = v.y;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V2F;
      u._float = new float[2];
      u._float[0] = v.x;
      u._float[1] = v.y;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V3I v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v.x;
      u._int[1] = v.y;
      u._int[2] = v.z;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V3I;
      u._int = new int[3];
      u._int[0] = v.x;
      u._int[1] = v.y;
      u._int[2] = v.z;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V3B v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v.x?1:0;
      u._int[1] = v.y?1:0;
      u._int[2] = v.z?1:0;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V3B;
      u._int = new int[3];
      u._int[0] = v.x?1:0;
      u._int[1] = v.y?1:0;
      u._int[2] = v.z?1:0;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V3F v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v.x;
      u._float[1] = v.y;
      u._float[2] = v.z;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V3F;
      u._float = new float[3];
      u._float[0] = v.x;
      u._float[1] = v.y;
      u._float[2] = v.z;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V4I v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v.x;
      u._int[1] = v.y;
      u._int[2] = v.z;
      u._int[3] = v.w;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V4I;
      u._int = new int[4];
      u._int[0] = v.x;
      u._int[1] = v.y;
      u._int[2] = v.z;
      u._int[3] = v.w;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V4B v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._int[0] = v.x?1:0;
      u._int[1] = v.y?1:0;
      u._int[2] = v.z?1:0;
      u._int[3] = v.w?1:0;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V4B;
      u._int = new int[4];
      u._int[0] = v.x?1:0;
      u._int[1] = v.y?1:0;
      u._int[2] = v.z?1:0;
      u._int[3] = v.w?1:0;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, V4F v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v.x;
      u._float[1] = v.y;
      u._float[2] = v.z;
      u._float[3] = v.w;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.V4F;
      u._float = new float[4];
      u._float[0] = v.x;
      u._float[1] = v.y;
      u._float[2] = v.z;
      u._float[3] = v.w;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, M22F v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v.x.x;
      u._float[1] = v.x.y;
      u._float[2] = v.y.x;
      u._float[3] = v.y.y;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.M22F;
      u._float = new float[4];
      u._float[0] = v.x.x;
      u._float[1] = v.x.y;
      u._float[2] = v.y.x;
      u._float[3] = v.y.y;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, M33F v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v.x.x;
      u._float[1] = v.x.y;
      u._float[2] = v.x.z;
      u._float[3] = v.y.x;
      u._float[4] = v.y.y;
      u._float[5] = v.y.z;
      u._float[6] = v.z.x;
      u._float[7] = v.z.y;
      u._float[8] = v.z.z;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.M33F;
      u._float = new float[9];
      u._float[0] = v.x.x;
      u._float[1] = v.x.y;
      u._float[2] = v.x.z;
      u._float[3] = v.y.x;
      u._float[4] = v.y.y;
      u._float[5] = v.y.z;
      u._float[6] = v.z.x;
      u._float[7] = v.z.y;
      u._float[8] = v.z.z;
      uniforms.put(name, u);
    }
  }

  public void setUniform(String name, M44F v) throws Exception {
    if (uniforms.containsKey(name)) {
      UniformValue u = uniforms.get(name);
      u._float[0] = v.x.x;
      u._float[1] = v.x.y;
      u._float[2] = v.x.z;
      u._float[3] = v.x.w;
      u._float[4] = v.y.x;
      u._float[5] = v.y.y;
      u._float[6] = v.y.z;
      u._float[7] = v.y.w;
      u._float[8] = v.z.x;
      u._float[9] = v.z.y;
      u._float[10] = v.z.z;
      u._float[11] = v.z.w;
      u._float[12] = v.w.x;
      u._float[13] = v.w.y;
      u._float[14] = v.w.z;
      u._float[15] = v.w.w;
    } else {
      UniformValue u = new UniformValue();
      u.tag = InputType.Tag.M44F;
      u._float = new float[16];
      u._float[0] = v.x.x;
      u._float[1] = v.x.y;
      u._float[2] = v.x.z;
      u._float[3] = v.x.w;
      u._float[4] = v.y.x;
      u._float[5] = v.y.y;
      u._float[6] = v.y.z;
      u._float[7] = v.y.w;
      u._float[8] = v.z.x;
      u._float[9] = v.z.y;
      u._float[10] = v.z.z;
      u._float[11] = v.z.w;
      u._float[12] = v.w.x;
      u._float[13] = v.w.y;
      u._float[14] = v.w.z;
      u._float[15] = v.w.w;
      uniforms.put(name, u);
    }
  }

}

