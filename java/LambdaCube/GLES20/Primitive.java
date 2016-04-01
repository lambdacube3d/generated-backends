package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
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

