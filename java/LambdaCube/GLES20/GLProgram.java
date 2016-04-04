package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class GLProgram {
  public Integer program, vertexShader, fragmentShader;
  public HashMap<String,Integer> programUniforms, programInTextures;
  public HashMap<String,StreamInfo> programStreams;
  public GLProgram() throws Exception {
    
    programUniforms = new HashMap<String,Integer>();
    programInTextures = new HashMap<String,Integer>();
    programStreams = new HashMap<String,StreamInfo>();

  }

}

