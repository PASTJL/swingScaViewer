
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

package com.jlp.scaviewer.commons.utils
import scala.collection.mutable._
import java.awt.Color
import scala.swing.SimpleSwingApplication
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing._
import java.awt.Font
import language.postfixOps
class Couleurs  {
  
  val tub: Tuple2[Color, Boolean] = (Color.RED, true)
  var colors: HashMap[Int, Tuple2[Color, Boolean]] = HashMap()

  colors += ((0, (new Color(0, 0, 0), true)))
  colors += ((1, (new Color(255, 0, 0), true)))
  colors += ((2, (new Color(0, 0, 255), true)))
  colors += ((3, (new Color(0, 119, 60), true)))

  colors += ((4, (new Color(100, 100, 100), true)))
  colors += ((5, (new Color(238, 89, 126), true)))
  colors += ((6, (new Color(0, 255, 0), true)))
  colors += ((7, (new Color(72, 176, 253), true)))

  colors += ((8, (new Color(150, 150, 150), true)))
  colors += ((9, (new Color(254, 158, 94), true)))
  colors += ((10, (new Color(0, 102, 51), true)))
  colors += ((11, (new Color(43, 123, 132), true)))

  colors += ((12, (new Color(175, 175, 175), true)))
  colors += ((13, (new Color(174, 13, 62), true)))
  colors += ((14, (new Color(18, 134, 15), true)))
  colors += ((15, (new Color(58, 236, 250), true)))

  colors += ((16, (new Color(70, 70, 70), true)))
  colors += ((17, (new Color(108, 2, 32), true)))
  colors += ((18, (new Color(120, 243, 138), true)))
  colors += ((19, (new Color(2, 111, 242), true)))

  var indice: Int = -1

 def colorIsNotBusy(col:Color):Boolean ={
    colorExists(col) && colors(indice)._2
    
  }
  def setBusyColor(col:Color)
  {
    if(colorExists(col))
    {
     colors +=   ((indice, (col, false)))
    }
  }
  
  def colorExists(col: Color): Boolean =
    {
      colors exists { x =>

        if (((x _2) _1) == col) {
          indice = (x _1)
          true
        } else {
          indice = -1
          false
        }

      }
    }

 
  
  def pickColor(): Color =
    {
      var retColor: Color = new Color(234, 11, 172)
      var bool: Boolean = true
      var i=0
      while(bool && i < colors.size) {
        if (((colors.get(i) get) _2) == true) {
          retColor = ((colors.get(i) get) _1)
          bool = false
          colors += ((i, (retColor, false)))
        }
        i+=1
      }

      retColor
    }

  def restoreColor(col: Color): Int = {
    var ret = -1
    if (colorExists(col)) {
      colors += ((indice, (col, true)))
      ret = indice
      indice = -1

    }
    ret
  }
  def restoreAllColors()
  {
    
   colors=colors map { x => (x._1,(x._2._1,true))}
  }
}
object Couleurs extends SimpleSwingApplication{
  var coul=new Couleurs()
 // println(coul.colorExists(new Color(70, 70, 70)))
  //println(coul.colorExists(new Color(10, 70, 70)))
  var col = coul.pickColor
 // println("Color picked=" + col)
  var ind1=coul  restoreColor(col)
  //println("Color restored  =  "+ind1+" -> "+(coul.colors.get(ind1)).get)
  val mainPanel = new BoxPanel(Orientation.Vertical)
  def top = new MainFrame {
    title = "ScaViewer Application Version 0.1"

    contents = mainPanel
    for (i <- 0 until coul.colors.size) {
      var tf = new TextField(5)
      tf.text=("Hello")
      tf.background = ((coul.colors.get(i) get) _1)
       tf.foreground = invertColor ( ((coul.colors.get(i) get) _1),150)
       tf.font=new Font("Arial",Font.BOLD,12);
      mainPanel.contents += tf

    }
    repaint

  }
 def invertColor(col:Color,decal:Int=150):Color=
  {
    var ret=new Color((col.getRed+decal)%255,(col.getGreen+decal)%255,(col.getBlue+decal)%255)
    var red=col.getRed
    var green=col.getGreen
    var blue=col.getBlue
    if(red <10 && green<50 && blue <50)
    {
      ret=new Color(255,255,255)
    }
    else if(red>200 &&  green<50 && blue <50 )
    {
       ret=new Color(255,255,255)
    }
    
    else if(red< 50 &&  green<50 && blue > 200 )
    {
       ret=new Color(255,255,255)
    }
    ret
  }
}
 
  