(* ::Package:: *)

Switch[$OperatingSystem,
  "MacOSX", (* Compilation settings for OS X *)
  {
	"CompileOptions" -> {
		" -Wall"
		,"-Wextra"
		,"-Wno-unused-parameter"
		,"-mmacosx-version-min="<>StringSplit[Import["!sw_vers &2>1","Text"]][[4]]
		,"-std=c++20"
		,Switch[$SystemID
			,"MacOSX-ARM64","-mcpu=native -mtune=native"
			,"MacOSX-x86-64","-march=native -mtune=native"
			,_,""
		]
		,"-ffast-math"
		,"-fno-math-errno"
		,"-Ofast"
		,"-flto"
		,"-pthread"
		,"-gline-tables-only"
		,"-gcolumn-info"
		,"-framework Accelerate"
		(*,"-ftime-trace"*)
(*		,"-foptimization-record-file="<>FileNameJoin[{$HomeDirectory,"RepulsorLink_OptimizationRecord.txt"}]
		,"-foptimization-record-passes=loop-vectorize*"
		,"-Rpass-analysis=loop-distribute"
		,"-Rpass-analysis=loop-vectorize"
		,"-Rpass-missed=loop-vectorize"
		,"-Rpass=loop-vectorize"*)
		}
	,"LinkerOptions"->{
		"-lm"
		,"-ldl"
		(*,"-lblas"*)
		(*,"-llapack"*)
		(*,"-lopenblas"*)
		}
    ,"IncludeDirectories" -> {
		FileNameJoin[{DirectoryName[$InputFileName],"LibraryResources","Source"}]
	    }
	,"LibraryDirectories" -> {
    }
    (*,"ShellCommandFunction" -> Print*)
	,"ShellOutputFunction" -> Print
 },

  "Unix", (* Compilation settings for Linux *)
  {
	"CompileOptions" -> {
		" -Wall"
		,"-Wextra"
		,"-Wno-unused-parameter"
		,"-std=c++20"
		,"-march=native -mtune=native"
		,"-ffast-math"
		,"-fno-math-errno"
		,"-Ofast"
		,"-flto"
		,"-pthread"
		,"-gline-tables-only"
		,"-gcolumn-info"
		}
	,"LinkerOptions"->{"-lm","-ldl","-lblas","-llapack"}
    ,"IncludeDirectories" -> {
		FileNameJoin[{ParentDirectory[$InputFileName],"LibraryResources","Source"}],
		$OpenBLASIncludeDirectory
	    }
	,"LibraryDirectories" -> {
		$OpenBLASLibraryDirectory
    }
    ,"ShellOutputFunction" -> Print
  },

  "Windows", (* Compilation settings for Windows *)
  {
    "CompileOptions" -> {"/EHsc", "/wd4244", "/DNOMINMAX", "/arch:AVX"}
    
    ,"IncludeDirectories" -> {
		FileNameJoin[{ParentDirectory[$InputFileName],"LibraryResources","Source"}],
		$OpenBLASIncludeDirectory
	    }
	,"LibraryDirectories" -> {
		$OpenBLASLibraryDirectory
	}
    ,"ShellOutputFunction" -> Print
  }
]
