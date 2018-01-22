/*Copyright 2012 Jean-Louis PASTUREL 
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*   See the License for the specific language governing permissions and
*  limitations under the License.
*/


package com.jlp.scaviewer.commons.utils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
object CopyFile {

  def copy(inputFile: File, outputFile: File) {
    val in = new FileInputStream(inputFile)
    val out = new FileOutputStream(outputFile)
    var c: Int = 0
    var buf: Array[Byte] = new Array[Byte](16384)
    var bool = true
    while (bool) {
      c = in.read(buf)
      if (c != -1)

        out.write(buf, 0, c);

      else
        bool = false
    }

    in.close();
    out.flush();

    out.close();

  }

}
