package LambdaCube.GLES20;

import java.nio.*;
import android.opengl.GLES20;
import LambdaCube.IR.*;
import RT.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class StreamMap {
  public HashMap<String,Stream> map;
  public StreamMap() throws Exception {
    
    map = new HashMap<String,Stream>();

  }

  public void add(String name, Float v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, V2F v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, V3F v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, V4F v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, M22F v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, M33F v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, M44F v) throws Exception {
    Stream s = new Stream(v);
    map.put(name, s);
  }

  public void add(String name, Type t, GLBuffer b, Integer index) throws Exception {
    Stream s = new Stream(b, index, t);
    map.put(name, s);
  }

  public Boolean validate() throws Exception {
    return true;
  }

}

