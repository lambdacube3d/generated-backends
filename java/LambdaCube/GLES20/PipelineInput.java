package LambdaCube.GLES20;

import android.opengl.GLES20;
import LambdaCube.IR.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class PipelineInput {
  public HashMap<String,ArrayList<Object>> objectMap;
  public HashMap<String,UniformValue> uniforms;
  public Integer screenWidth, screenHeight;
  public Object createObject(String slotName, Primitive prim, StreamMap attributes, ArrayList<String> objectUniforms) {
    Object o = new Object();
    o.enabled = true;
    o.order = 0;
    o.glMode = primitiveMode(prim);
    Integer count = 0;
    for (Map.Entry<String,Stream> i : attributes.map.entrySet()) {
      if (i.getValue().isArray) {
        count = i.getValue().buffer.size.get(i.getValue().index) / i.getValue().glSize;
        break;
      }
    }
    o.glCount = count;
    o.streams = attributes;
    if (objectMap.containsKey(slotName)) {
      objectMap.get(slotName).add(o);
    } else {
      ArrayList<Object> ov = new ArrayList<Object>();
      ov.add(o);
      objectMap.put(slotName, ArrayList<Object>(ov));
    }
    return o;
  }

  public Object createObject(String slotName, Primitive prim, StreamMap attributes, Buffer indexBuffer, Integer bufferIndex, ArrayList<String> objectUniforms) {
    Object o = new Object();
    return o;
  }

  public void sortSlotObjects() {
  }

  public void setScreenSize(Integer w, Integer h) {
    screenWidth = w;
    screenHeight = h;
  }

  public void setUniform(String name, Integer v) {
    uniforms.put(name, {.tag = InputType.Tag.Int, ._int = v});
  }

  public void setUniform(String name, Integer v) {
    uniforms.put(name, {.tag = InputType.Tag.Word, ._word = v});
  }

  public void setUniform(String name, Float v) {
    uniforms.put(name, {.tag = InputType.Tag.Float, ._float = v});
  }

  public void setUniform(String name, Boolean v) {
    uniforms.put(name, {.tag = InputType.Tag.Bool, ._bool = v});
  }

  public void setUniform(String name, V2I v) {
    uniforms.put(name, {.tag = InputType.Tag.V2I, ._v2i = v});
  }

  public void setUniform(String name, V2U v) {
    uniforms.put(name, {.tag = InputType.Tag.V2U, ._v2u = v});
  }

  public void setUniform(String name, V2F v) {
    uniforms.put(name, {.tag = InputType.Tag.V2F, ._v2f = v});
  }

  public void setUniform(String name, V2B v) {
    uniforms.put(name, {.tag = InputType.Tag.V2B, ._v2b = v});
  }

  public void setUniform(String name, V3I v) {
    uniforms.put(name, {.tag = InputType.Tag.V3I, ._v3i = v});
  }

  public void setUniform(String name, V3U v) {
    uniforms.put(name, {.tag = InputType.Tag.V3U, ._v3u = v});
  }

  public void setUniform(String name, V3F v) {
    uniforms.put(name, {.tag = InputType.Tag.V3F, ._v3f = v});
  }

  public void setUniform(String name, V3B v) {
    uniforms.put(name, {.tag = InputType.Tag.V3B, ._v3b = v});
  }

  public void setUniform(String name, V4I v) {
    uniforms.put(name, {.tag = InputType.Tag.V4I, ._v4i = v});
  }

  public void setUniform(String name, V4U v) {
    uniforms.put(name, {.tag = InputType.Tag.V4U, ._v4u = v});
  }

  public void setUniform(String name, V4F v) {
    uniforms.put(name, {.tag = InputType.Tag.V4F, ._v4f = v});
  }

  public void setUniform(String name, V4B v) {
    uniforms.put(name, {.tag = InputType.Tag.V4B, ._v4b = v});
  }

  public void setUniform(String name, M22F v) {
    uniforms.put(name, {.tag = InputType.Tag.M22F, ._m22f = v});
  }

  public void setUniform(String name, M33F v) {
    uniforms.put(name, {.tag = InputType.Tag.M33F, ._m33f = v});
  }

  public void setUniform(String name, M44F v) {
    uniforms.put(name, {.tag = InputType.Tag.M44F, ._m44f = v});
  }

}

