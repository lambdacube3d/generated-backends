package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Stream {
  public Type type;
  public GLBuffer buffer;
  public Integer index;
  public Boolean isArray;
  public Integer glSize;
  public UniformValue attributeValue;
  public Stream(Float v) throws Exception {
    type = Type.FLOAT;
    isArray = false;
    glSize = 1;
  }

  public Stream(V2F v) throws Exception {
    type = Type.FLOAT_VEC2;
    isArray = false;
    glSize = 2;
  }

  public Stream(V3F v) throws Exception {
    type = Type.FLOAT_VEC3;
    isArray = false;
    glSize = 3;
  }

  public Stream(V4F v) throws Exception {
    type = Type.FLOAT_VEC4;
    isArray = false;
    glSize = 4;
  }

  public Stream(M22F v) throws Exception {
    type = Type.FLOAT_MAT2;
    isArray = false;
    glSize = 4;
  }

  public Stream(M33F v) throws Exception {
    type = Type.FLOAT_MAT3;
    isArray = false;
    glSize = 9;
  }

  public Stream(M44F v) throws Exception {
    type = Type.FLOAT_MAT4;
    isArray = false;
    glSize = 16;
  }

  public Stream(GLBuffer b, Integer i, Type t) throws Exception {
    type = t;
    buffer = b;
    index = i;
    isArray = true;
    glSize = 16;
    switch (t) {
      case FLOAT: glSize = 1; break;
      case FLOAT_VEC2: glSize = 2; break;
      case FLOAT_VEC3: glSize = 3; break;
      case FLOAT_VEC4: glSize = 4; break;
      case FLOAT_MAT2: glSize = 4; break;
      case FLOAT_MAT3: glSize = 9; break;
      case FLOAT_MAT4: glSize = 16; break;
    }
  }

}

