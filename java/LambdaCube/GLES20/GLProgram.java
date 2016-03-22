package LambdaCube.GLES20;

import android.opengl.GLES20;
import LambdaCube.IR.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class GLProgram {
  public Integer program, vertexShader, fragmentShader;
  public HashMap<String,Integer> programUniforms, programInTextures;
  public HashMap<String,StreamInfo> programStreams;
}

