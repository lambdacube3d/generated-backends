package LambdaCube.GLES20;

import android.opengl.GLES20;
import LambdaCube.IR.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Buffer {
  public ArrayList<Integer> size, byteSize, glType;
  public ArrayList<Integer> offset;
  public ArrayList<void> data;
  public Integer bufferObject;
  public Integer add(ArrayList<Integer> v) throws Exception {
    Integer i = data.size();
    data.add(v.data());
    size.add(v.size());
    byteSize.add(v.size());
    glType.add(GLES20.GL_BYTE);
    return i;
  }

  public Integer add(ArrayList<Integer> v) throws Exception {
    Integer i = data.size();
    data.add(v.data());
    size.add(v.size());
    byteSize.add(v.size());
    glType.add(GLES20.GL_UNSIGNED_BYTE);
    return i;
  }

  public Integer add(ArrayList<Integer> v) throws Exception {
    Integer i = data.size();
    data.add(v.data());
    size.add(v.size());
    byteSize.add(2 * v.size());
    glType.add(GLES20.GL_SHORT);
    return i;
  }

  public Integer add(ArrayList<Integer> v) throws Exception {
    Integer i = data.size();
    data.add(v.data());
    size.add(v.size());
    byteSize.add(2 * v.size());
    glType.add(GLES20.GL_UNSIGNED_SHORT);
    return i;
  }

  public Integer add(ArrayList<Float> v) throws Exception {
    Integer i = data.size();
    data.add(v.data());
    size.add(v.size());
    byteSize.add(4 * v.size());
    glType.add(GLES20.GL_FLOAT);
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

