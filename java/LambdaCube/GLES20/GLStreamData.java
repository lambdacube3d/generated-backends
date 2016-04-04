package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class GLStreamData {
  public Integer glMode, glCount;
  public StreamMap streams;
  public GLStreamData() throws Exception {
    
    streams = new StreamMap();

  }

}

