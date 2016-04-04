package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class PipelineInput {
  public HashMap<String,ArrayList<Object>> objectMap;
  public HashMap<String,UniformValue> uniforms;
  public Integer screenWidth, screenHeight;
  public PipelineInput() throws Exception {
    
    objectMap = new HashMap<String,ArrayList<Object>>();
    uniforms = new HashMap<String,UniformValue>();

  }

  public Object createObject(String slotName, Primitive prim, StreamMap attributes, ArrayList<String> objectUniforms) throws Exception {
    Object o = new Object();
    o.enabled = true;
    o.order = 0;
    o.glMode = Util.primitiveMode(prim);
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
      objectMap.put(slotName, ov);
    }
    return o;
  }

  public Object createObject(String slotName, Primitive prim, StreamMap attributes, GLBuffer indexBuffer, Integer bufferIndex, ArrayList<String> objectUniforms) throws Exception {
    Object o = new Object();
    return o;
  }

  public void sortSlotObjects() throws Exception {
  }

  public void setScreenSize(Integer w, Integer h) throws Exception {
    screenWidth = w;
    screenHeight = h;
  }

}

