import sys
import os
import os.path
import ntpath
import uuid
import string
import glob

template = r"""<?xml version="1.0" encoding="utf-8"?>
<Project DefaultTargets="Build" ToolsVersion="4.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <ItemGroup Label="ProjectConfigurations">
    <ProjectConfiguration Include="Debug|Win32">
      <Configuration>Debug</Configuration>
      <Platform>Win32</Platform>
    </ProjectConfiguration>
    <ProjectConfiguration Include="Debug|x64">
      <Configuration>Debug</Configuration>
      <Platform>x64</Platform>
    </ProjectConfiguration>
    <ProjectConfiguration Include="Release|Win32">
      <Configuration>Release</Configuration>
      <Platform>Win32</Platform>
    </ProjectConfiguration>
    <ProjectConfiguration Include="Release|x64">
      <Configuration>Release</Configuration>
      <Platform>x64</Platform>
    </ProjectConfiguration>
  </ItemGroup>
  <PropertyGroup Label="Globals">
    <ProjectName>$package</ProjectName>
    <ProjectGuid>{$guid}</ProjectGuid>
    <DisableFastUpToDateCheck>true</DisableFastUpToDateCheck>
    <WindowsTargetPlatformVersion>10.0.16299.0</WindowsTargetPlatformVersion>
  </PropertyGroup>
  <Import Project="$$(VCTargetsPath)\Microsoft.Cpp.Default.props" />
  <PropertyGroup Label="Configuration">
    <ConfigurationType>Application</ConfigurationType>
    <PlatformToolset Condition="'$$(VisualStudioVersion)' == '11.0' Or '$$(PlatformToolsetVersion)' == '110' Or '$$(MSBuildToolsVersion)' ==  '4.0'">v110_xp</PlatformToolset>
    <PlatformToolset Condition="'$$(VisualStudioVersion)' == '12.0' Or '$$(PlatformToolsetVersion)' == '120' Or '$$(MSBuildToolsVersion)' == '12.0'">v120_xp</PlatformToolset>
    <PlatformToolset Condition="'$$(VisualStudioVersion)' == '14.0' Or '$$(PlatformToolsetVersion)' == '140' Or '$$(MSBuildToolsVersion)' == '14.0'">v140</PlatformToolset>
    <PlatformToolset Condition="'$$(VisualStudioVersion)' == '15.0' Or '$$(PlatformToolsetVersion)' == '141' Or '$$(MSBuildToolsVersion)' == '15.0'">v141</PlatformToolset>
    <UseOfMfc>false</UseOfMfc>
    <CharacterSet>MultiByte</CharacterSet>
    <WholeProgramOptimization Condition="'$$(Configuration)'=='Release'">true</WholeProgramOptimization>
    <UseDebugLibraries Condition="'$$(Configuration)'=='Debug'">true</UseDebugLibraries>
  </PropertyGroup>
  <Import Project="$$(VCTargetsPath)\Microsoft.Cpp.props" />
  <ImportGroup Label="PropertySheets">
    <Import Project="$$(UserRootDir)\Microsoft.Cpp.$$(Platform).user.props" Condition="exists('$$(UserRootDir)\Microsoft.Cpp.$$(Platform).user.props')" Label="LocalAppDataPlatform" />
  </ImportGroup>
  <PropertyGroup>
    <OutDir Condition="'$$(Platform)'=='x64'">$$(Configuration)\64bit\</OutDir>
    <OutDir Condition="'$$(Platform)'=='Win32'">$$(Configuration)\32bit\</OutDir>
    <IntDir>$$(OutDir)</IntDir>
    <GenerateManifest>false</GenerateManifest>
    <LinkIncremental Condition="'$$(Configuration)'=='Debug'">true</LinkIncremental>
    <LinkIncremental Condition="'$$(Configuration)'=='Release'">false</LinkIncremental>
    <DebuggerFlavor>WindowsLocalDebugger</DebuggerFlavor>
  </PropertyGroup>
  <ItemDefinitionGroup>
    <ClCompile>
      <BufferSecurityCheck>false</BufferSecurityCheck>
      <WarningLevel>Level3</WarningLevel>
      <SuppressStartupBanner>true</SuppressStartupBanner>
      <ExceptionHandling>false</ExceptionHandling>
      <RuntimeTypeInfo>false</RuntimeTypeInfo>
      <PreprocessorDefinitions>WIN32;_WINDOWS;%(PreprocessorDefinitions)</PreprocessorDefinitions>
      <PreprocessorDefinitions>_HAS_EXCEPTIONS=0;_CRT_SECURE_NO_WARNINGS;_CRT_NONSTDC_NO_WARNINGS;%(PreprocessorDefinitions)</PreprocessorDefinitions>
      <PreprocessorDefinitions Condition="'$$(Configuration)'=='Debug'">_DEBUG;%(PreprocessorDefinitions)</PreprocessorDefinitions>
      <PreprocessorDefinitions Condition="'$$(Configuration)'=='Release'">NDEBUG;%(PreprocessorDefinitions)</PreprocessorDefinitions>
      <PreprocessorDefinitions Condition="'$$(Platform)'=='x64'">WIN64;_WIN64;%(PreprocessorDefinitions)</PreprocessorDefinitions>
    </ClCompile>
    <ClCompile Condition="'$$(Configuration)'=='Debug'">
      <Optimization>Disabled</Optimization>
      <BasicRuntimeChecks>EnableFastChecks</BasicRuntimeChecks>
      <RuntimeLibrary>MultiThreadedDebug</RuntimeLibrary>
    </ClCompile>
    <ClCompile Condition="'$$(Configuration)'=='Release'">
      <Optimization>Full</Optimization>
      <StringPooling>true</StringPooling>
      <IntrinsicFunctions>true</IntrinsicFunctions>
      <RuntimeLibrary>MultiThreaded</RuntimeLibrary>
      <FunctionLevelLinking>true</FunctionLevelLinking>
      <OmitFramePointers>true</OmitFramePointers>
      <IntrinsicFunctions>true</IntrinsicFunctions>
      <FavorSizeOrSpeed>Speed</FavorSizeOrSpeed>
      <WholeProgramOptimization>true</WholeProgramOptimization>
      <InlineFunctionExpansion>AnySuitable</InlineFunctionExpansion>
      <FloatingPointExceptions>false</FloatingPointExceptions>
      <EnableEnhancedInstructionSet Condition="'$$(Platform)'=='Win32'">StreamingSIMDExtensions2</EnableEnhancedInstructionSet>
      <AdditionalOptions Condition="'$$(VisualStudioVersion)' &gt;= '12.0' Or '$$(PlatformToolsetVersion)' &gt;= '120' Or '$$(MSBuildToolsVersion)' &gt;= '12.0'">/Gw %(AdditionalOptions)</AdditionalOptions>
    </ClCompile>
    <Link>
      <SuppressStartupBanner>true</SuppressStartupBanner>
      <GenerateDebugInformation>true</GenerateDebugInformation>
      <SubSystem>Console</SubSystem>
      <ImageHasSafeExceptionHandlers>false</ImageHasSafeExceptionHandlers>
    </Link>
    <Link Condition="'$$(Configuration)'=='Release'">
      <EnableCOMDATFolding>true</EnableCOMDATFolding>
      <OptimizeReferences>true</OptimizeReferences>
      <LinkTimeCodeGeneration>UseLinkTimeCodeGeneration</LinkTimeCodeGeneration>
      <IgnoreEmbeddedIDL>true</IgnoreEmbeddedIDL>
      <GenerateWindowsMetadata>false</GenerateWindowsMetadata>
    </Link>
    <CustomBuildStep>
      <Command>
        cd $ionhome
        ion $flags $package
      </Command>
    </CustomBuildStep>
  </ItemDefinitionGroup>
  <Import Project="$$(VCTargetsPath)\Microsoft.Cpp.targets" />
  <ItemGroup>
    <ClCompile Include="$cfile" />
  </ItemGroup>
  <ItemGroup>
    $ionfiles
  </ItemGroup>
</Project>"""

base_guid = uuid.UUID('14165d8a-5f52-499b-b8b8-41638cdf0643')

if len(sys.argv) < 2:
    print("Usage: %s <ion-package> [ion-flags]" % sys.argv[0])
    sys.exit(1)

package = sys.argv[1]
flags = sys.argv[2:]
ionhome = os.getenv("IONHOME");
ionpath = os.getenv("IONPATH");

packagepath = package.replace('.', '\\')
for base in ionpath.split(';'):
    path = ntpath.join(base, packagepath)
    if os.path.isdir(path):
        packagepath = path
        break
else:
    print("Package %s not found in %%IONPATH%%" % package)
    sys.exit(1)

template = string.Template(template)
guid = uuid.uuid3(base_guid, package)
cfile = ntpath.join(ntpath.abspath(os.getcwd()), package, "%s.c" % (package))
flags = ' '.join(["-o " + cfile] + flags)
ionfilelist = glob.glob(ntpath.join(packagepath, "*.ion"))
ionfiles = '\n'.join("""    <Text Include="%s" />""" % ionfile for ionfile in ionfilelist)
template = template.substitute(
  guid=guid,
  package=package,
  ionhome=ionhome,
  cfile=cfile,
  packagepath=packagepath,
  ionfiles=ionfiles,
  flags = flags,
)

if not os.path.isdir(package):
    os.mkdir(package)

vcxproj = open("%s/%s.vcxproj" % (package, package), "w")
vcxproj.write(template)

