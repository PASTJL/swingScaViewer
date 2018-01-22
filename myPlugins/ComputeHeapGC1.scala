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
import java.util.HashMap

class ComputeHeapGC1 {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    ComputeHeapGC1.currentEnreg=""
     ComputeHeapGC1. hmResult=null 
  }

 def retour(tabStr:Array[String]):Double=
  {
   
		var retour=Double.NaN
		// System.out.println(" enr =|" + tabString[0] + "|");
		// String[0] => Enreg a traiter

		// 2 type de temps a recuperer sur un Young GC ou sur un full GC

		// System.out.println("rentree dans ComputeHeapGC1");
		// System.out.println("enr=|" + currentEnreg + "|");
		// System.out.println("Valeur traitee=" + tabString[1]);
		if (!ComputeHeapGC1.currentEnreg.equals(tabStr(0))) {
			// System.out.println("Creation hashMap ComputeHeapGC1 coucou0");
			// recree le nouveau tableau
			ComputeHeapGC1.currentEnreg = tabStr(0)
			ComputeHeapGC1.hmResult = new HashMap[String, Double]();

			// BeforeGCHeapTotal
			var regex1="garbage-first\\s+heap\\s+total\\s+\\d+".r
			
			// System.out.println("Creation hashMap ComputeHeapGC1 coucou1");
			
			var ext = regex1.findFirstIn(tabStr(0))
			var reste = "";
			if (None != ext) {
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou2");
				
				reste = tabStr(0).substring(ext.get.length())
				var regex2 = "\\d+$".r
				var ext3=regex2.findFirstIn(ext.get)
				
				if (None != ext3) {
					ComputeHeapGC1.hmResult.put("BeforeGCHeapTotal", ext3.get.toDouble)
				} else {
					ComputeHeapGC1.hmResult.put("BeforeGCHeapTotal", Double.NaN)
					;
				}

			} else {
				ComputeHeapGC1.hmResult.put("BeforeGCHeapTotal", Double.NaN)
				
			}
			// System.out.println("Creation hashMap ComputeHeapGC1 coucou3");
			// BeforeFullGCHeapUsed
			// Test si full GC
			if (tabStr(0).indexOf("Full GC") >= 0) {
				ComputeHeapGC1.hmResult.put("BeforeYoungGCHeapUsed",  Double.NaN);

				ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenTotal",  Double.NaN);

				ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenUsed",  Double.NaN);

				ComputeHeapGC1.hmResult.put("AfterYoungGCHeapTotal",  Double.NaN);

				ComputeHeapGC1.hmResult.put("AfterYoungGCHeapUsed",  Double.NaN);

				ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenTotal",  Double.NaN);

				ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenUsed",  Double.NaN);
				ComputeHeapGC1.hmResult.put("DurationYoungGC",  Double.NaN);
				ComputeHeapGC1.hmResult.put("ParallelTime",  Double.NaN);

				// System.out.println("Creation hashMap ComputeHeapGC1 coucou4");
				regex1 ="used\\s\\d+".r
				ext=regex1.findFirstIn(reste)
				
				if (None != ext) {
					
					reste = tabStr(0).substring(tabStr(0).indexOf(ext.get)
							+ ext.get.length());
					var regex2 = "\\d+$".r
					
					var ext2=regex2.findFirstIn(ext.get)
					if (None != ext2) {
						ComputeHeapGC1.hmResult.put("BeforeFullGCHeapUsed", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("BeforeFullGCHeapUsed", Double.NaN)
						
					}

				} else {
					ComputeHeapGC1.hmResult.put("BeforeFullGCHeapUsed", Double.NaN)
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou5");
				// BeforeFullGCPermGenUsed
				regex1 ="used\\s\\d+".r
				
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					var regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2) {
						ComputeHeapGC1.hmResult.put("BeforeFullGCPermGenUsed", ext2.get.toDouble);
					} else {
						ComputeHeapGC1.hmResult.put("BeforeFullGCPermGenUsed",  Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("BeforeFullGCPermGenUsed",  Double.NaN);

				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou6");
				// Duration Full GC
				regex1 ="\\[Full\\s+GC[^\\,]+,\\s+\\d+\\.\\d+".r
				
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+\\.\\d$".r
					val ext2 = regex2.findFirstIn(ext.get)
					if (None != ext2) {
						ComputeHeapGC1.hmResult.put("DurationFullGC",ext2.get.toDouble);
					} else {
						ComputeHeapGC1.hmResult.put("DurationFullGC",  Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("DurationFullGC", Double.NaN)

				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou7");
				// NbYoungGC
				regex1 = "after\\s+GC\\s+invocations=\\d+".r
				
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("NbYoungGC",ext2.get.toDouble);
					} else {
						ComputeHeapGC1.hmResult.put("NbYoungGC", Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("NbYoungGC",  Double.NaN)

				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou8");
				// NbFullGC
				regex1 = "\\(full\\s*\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("NbFullGC",ext2.get.toDouble);
					} else {
						ComputeHeapGC1.hmResult.put("NbFullGC",  Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("NbFullGC", Double.NaN)

				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou9");
				// AfterFullGCHeapUsed
				regex1 ="used\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("AfterFullGCHeapUsed",ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("AfterFullGCHeapUsed", Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("AfterFullGCHeapUsed",  Double.NaN)

				}

				// System.out.println("Creation hashMap ComputeHeapGC1 coucou10");
				// AfterFullGCPermGenUsed
				regex1 = "used\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("AfterFullGCPermGenUsed",ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("AfterFullGCPermGenUsed", Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("AfterFullGCPermGenUsed",  Double.NaN)

				}

			} else {
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou11");
				// traitement young GC
				ComputeHeapGC1.hmResult.put("BeforeFullGCHeapUsed",  Double.NaN)
				ComputeHeapGC1.hmResult.put("BeforeFullGCPermGenUsed",  Double.NaN)
				ComputeHeapGC1.hmResult.put("AfterFullGCHeapUsed", Double.NaN)
				ComputeHeapGC1.hmResult.put("AfterFullGCPermGenUsed", Double.NaN)
				ComputeHeapGC1.hmResult.put("DurationFullGC",  Double.NaN)
				// BeforeYoungGCHeapUsed
				regex1 = "used\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("BeforeYoungGCHeapUsed",ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("BeforeYoungGCHeapUsed",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("BeforeYoungGCHeapUsed", Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou12");
				// BeforeYoungGCPermGenTotal
				regex1="compacting\\s+perm\\s+gen\\s+total\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenTotal",ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenTotal",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenTotal",  Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou13");
				// BeforeYoungGCPermGenUsed
				regex1 ="used\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+$".r
					
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult
								.put("BeforeYoungGCPermGenUsed", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenUsed",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("BeforeYoungGCPermGenUsed",  Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou14");
				// DurationYoungGC
				regex1 = "\\[GC\\s+pause\\s+\\(young\\),\\s+\\d+\\.\\d+".r
			ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+\\.\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("DurationYoungGC",ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("DurationYoungGC",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("DurationYoungGC",  Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou15");
				// ParallelTime
				regex1 = "\\[Parallel\\s+Time:\\s+\\d+\\.\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 = "\\d+\\.\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("ParallelTime", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("ParallelTime",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("ParallelTime",  Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou16");
				// NbYoungGC
				regex1 = "after\\s+GC\\s+invocations=\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 ="\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("NbYoungGC", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("NbYoungGC", Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("NbYoungGC",Double.NaN)

				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou17");
				// NbFullGC
				regex1 = "\\(full\\s*\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 ="\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("NbFullGC", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("NbFullGC", Double.NaN)

					}

				} else {
					ComputeHeapGC1.hmResult.put("NbFullGC", Double.NaN)

				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou18");
				// AfterYoungGCHeapTotal
				regex1 = "garbage-first\\s+heap\\s+total\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 ="\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("AfterYoungGCHeapTotal", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("AfterYoungGCHeapTotal",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("AfterYoungGCHeapTotal", Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou19");
				// AfterYoungGCHeapUsed
				regex1 = "used\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 ="\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("AfterYoungGCHeapUsed", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("AfterYoungGCHeapUsed",  Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("AfterYoungGCHeapUsed", Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou20");
				// AfterYoungGCPermGenTotal
				regex1= "compacting\\s+perm\\s+gen\\s+total\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 ="\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult
								.put("AfterYoungGCPermGenTotal",  ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenTotal",Double.NaN)
						;
					}

				} else {
					ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenTotal", Double.NaN)
					;
				}
				// System.out.println("Creation hashMap ComputeHeapGC1 coucou21");
				// AfterYoungGCPermGenUsed
				regex1 ="used\\s+\\d+".r
				ext=regex1.findFirstIn(reste)
				if (None != ext) {
					
					reste = reste.substring(reste.indexOf(ext.get) + ext.get.length());
					val regex2 ="\\d+$".r
					val ext2=regex2.findFirstIn(ext.get)
					if (None != ext2 ) {
						ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenUsed", ext2.get.toDouble)
					} else {
						ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenUsed", Double.NaN)
						;
					}

				} else {
							ComputeHeapGC1.hmResult.put("AfterYoungGCPermGenUsed", Double.NaN)
					;
				}

			}

		}
		// le tableau de resultat est deja rempli on retourne la valeur.

		if (tabStr(1) == "BeforeGCHeapTotal") {
			return ComputeHeapGC1.hmResult.get("BeforeGCHeapTotal");
		}
		else if (tabStr(1) =="BeforeFullGCHeapUsed") {
			return ComputeHeapGC1.hmResult.get("BeforeFullGCHeapUsed");
		}

		else if (tabStr(1) =="BeforeFullGCPermGenUsed") {
			return ComputeHeapGC1.hmResult.get("BeforeFullGCPermGenUsed");
		}

		else if (tabStr(1) =="BeforeYoungGCHeapUsed") {
			return ComputeHeapGC1.hmResult.get("BeforeYoungGCHeapUsed");
		}
		else if (tabStr(1) =="BeforeYoungGCPermGenTotal") {
			return ComputeHeapGC1.hmResult.get("BeforeYoungGCPermGenTotal");
		}
		else if (tabStr(1) =="BeforeYoungGCPermGenUsed") {
			return ComputeHeapGC1.hmResult.get("BeforeYoungGCPermGenUsed");
		}
		else if (tabStr(1) =="NbYoungGC") {
			return ComputeHeapGC1.hmResult.get("NbYoungGC");
		}
		else if (tabStr(1) =="NbFullGC") {
			return ComputeHeapGC1.hmResult.get("NbFullGC");
		}
		else if (tabStr(1) =="AfterYoungGCHeapTotal") {
			return ComputeHeapGC1.hmResult.get("AfterYoungGCHeapTotal");
		}
		else if (tabStr(1) =="AfterYoungGCHeapUsed") {
			return ComputeHeapGC1.hmResult.get("AfterYoungGCHeapUsed");
		}
		else if (tabStr(1) =="AfterYoungGCPermGenTotal") {
			return ComputeHeapGC1.hmResult.get("AfterYoungGCPermGenTotal");
		}
		else if (tabStr(1) =="AfterYoungGCPermGenUsed") {
			return ComputeHeapGC1.hmResult.get("AfterYoungGCPermGenUsed");
		}
		else if (tabStr(1) =="ParallelTime") {
			return ComputeHeapGC1.hmResult.get("ParallelTime");
		}
		else if (tabStr(1) =="AfterFullGCHeapUsed") {
			return ComputeHeapGC1.hmResult.get("AfterFullGCHeapUsed");
		}
		else if (tabStr(1) =="AfterFullGCPermGenUsed") {
			return ComputeHeapGC1.hmResult.get("AfterFullGCPermGenUsed");
		}
		else if (tabStr(1) =="DurationFullGC") {
			return ComputeHeapGC1.hmResult.get("DurationFullGC");
		}

		else if (tabStr(1) =="DurationYoungGC") {
			return ComputeHeapGC1.hmResult.get("DurationYoungGC");
		}
		else 
		return Double.NaN

	
   
   
  }
}
object ComputeHeapGC1 
{
  
  var currentEnreg = ""
	var  hmResult:HashMap[String, Double]=null 
  }