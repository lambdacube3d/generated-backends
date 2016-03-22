package LambdaCube.GLES20;

import android.opengl.GLES20;
import LambdaCube.IR.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

enum Primitive {
  TriangleStrip,
  TriangleList,
  TriangleFan,
  LineStrip,
  LineList,
  LineLoop,
  PointList
};

