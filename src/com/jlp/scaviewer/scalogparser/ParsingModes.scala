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
package com.jlp.scaviewer.scalogparser

trait ParsingModes {

  final val SANSPIV = 100;
 
  final val PIV_REGEXP1_SANSREGEXP2 = 230;
  final val PIV_REGEXP1_REGEXP2 = 231;
  final val FASTPIV1STRING = 500;
 

  final val SANSVAL = 300;
 
  final val VAL_REGEXP1_SANSREGEXP2 = 430;
  final val VAL_REGEXP1_REGEXP2 = 431;
  final val VAL_FUNCTION = 440;

}