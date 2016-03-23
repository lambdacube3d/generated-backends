package LambdaCube.GLES20;

import android.opengl.GLES20;
import LambdaCube.IR.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Stream {
  public Type type;
  public Buffer buffer;
  public Integer index;
  public Boolean isArray;
  public Integer glSize;
  public Float _float;
  public V2F _v2f;
  public V3F _v3f;
  public V4F _v4f;
  public M22F _m22f;
  public M33F _m33f;
  public M44F _m44f;
  public Stream(Float v) throws Exception {
    type = Type.FLOAT;
    isArray = false;
    _float = v;
    glSize = 1;
  }

  public Stream(V2F v) throws Exception {
    type = Type.FLOAT_VEC2;
    isArray = false;
    _v2f = v;
    glSize = 2;
  }

  public Stream(V3F v) throws Exception {
    type = Type.FLOAT_VEC3;
    isArray = false;
    _v3f = v;
    glSize = 3;
  }

  public Stream(V4F v) throws Exception {
    type = Type.FLOAT_VEC4;
    isArray = false;
    _v4f = v;
    glSize = 4;
  }

  public Stream(M22F v) throws Exception {
    type = Type.FLOAT_MAT2;
    isArray = false;
    _m22f = v;
    glSize = 4;
  }

  public Stream(M33F v) throws Exception {
    type = Type.FLOAT_MAT3;
    isArray = false;
    _m33f = v;
    glSize = 9;
  }

  public Stream(M44F v) throws Exception {
    type = Type.FLOAT_MAT4;
    isArray = false;
    _m44f = v;
    glSize = 16;
  }

  public Stream(Buffer b, Integer i, Type t) throws Exception {
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

