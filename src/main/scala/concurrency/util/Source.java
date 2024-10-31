package util;

import java.io.IOException;
import java.io.InputStream;

/**
 * An abstraction around data sources from which one may get an InputStream. A
 * Source also has a String description for display purposes.
 * 
 * @author bhoward
 */
public interface Source {
  String getDescription();

  InputStream getInputStream() throws IOException;
}