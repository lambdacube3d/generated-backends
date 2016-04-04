package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class GLBuffer {
  public ArrayList<Integer> size, byteSize, glType;
  public ArrayList<Integer> offset;
  public ArrayList<Buffer> data;
  public Integer bufferObject;
  public GLBuffer() throws Exception {
    
    size = new ArrayList<Integer>();
    byteSize = new ArrayList<Integer>();
    glType = new ArrayList<Integer>();
    offset = new ArrayList<Integer>();
    data = new ArrayList<Buffer>();

  }

  public Integer add(Buffer buf, Integer elemGLType, Integer elemCount) throws Exception {
    Integer i = data.size();
    Integer elemSize = 1;
    switch (elemGLType) {
      case GLES20.GL_UNSIGNED_BYTE: elemSize = 1; break;
      case GLES20.GL_BYTE: elemSize = 1; break;
      case GLES20.GL_UNSIGNED_SHORT: elemSize = 2; break;
      case GLES20.GL_SHORT: elemSize = 2; break;
      default:
        elemSize = 4;

    }
    data.add(buf);
    size.add(elemCount);
    byteSize.add(elemSize * elemCount);
    glType.add(elemGLType);
    return i;
  }

  public void freeze() throws Exception {
    Integer bufferSize = 0;
    for (Integer i : byteSize) {
      offset.add(bufferSize);
      bufferSize += i;
    }
    Integer bo;
    { int[] glObj = new int[1]; GLES20.glGenBuffers(1, glObj, 0); bo = glObj[0]; }
    GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, bo);
    GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, bufferSize, null, GLES20.GL_STATIC_DRAW);
    Integer offset_ = 0;
    for (int i = 0; i < data.size(); i++) {
      GLES20.glBufferSubData(GLES20.GL_ARRAY_BUFFER, offset_, byteSize.get(i), data.get(i));
      offset_ += byteSize.get(i);
    }
    bufferObject = bo;
  }

  public void update(Integer i, ArrayList<Float> v) throws Exception {
  }

}

