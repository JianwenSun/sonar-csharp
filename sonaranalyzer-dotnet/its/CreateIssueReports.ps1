﻿$ErrorActionPreference = "Stop"

function FixUriDeclaration
{
	param ($files)

	$files |
      Foreach-Object {
        If ($_.uri) {
		  # Remove the URI prefix
          $_.uri = $_.uri.replace('file:///', '')
		  # Remove the common absolute path prefix
		  $_.uri = ([System.IO.FileInfo]$_.uri).FullName
          $_.uri = $_.uri.replace($pathPrefix, '')
          $_.uri = $_.uri.replace('/', '\')
        }
      }
}

function GetIssue
{
  param ($entry)

  $issue = New-Object –Type System.Object
  $issue | Add-Member –Type NoteProperty –Name id –Value $entry.ruleId
  If ($entry.shortMessage) {
    $issue | Add-Member –Type NoteProperty –Name message –Value $entry.shortMessage
  }
  ElseIf ($entry.fullMessage) {
    $issue | Add-Member –Type NoteProperty –Name message –Value $entry.fullMessage
  }
  $issue | Add-Member –Type NoteProperty –Name location –Value $entry.locations.analysisTarget

  $issue
  return
}

function GetIssueV3
{
  param ($entry)

  $issue = New-Object –Type System.Object
  $issue | Add-Member –Type NoteProperty –Name id –Value $entry.ruleId
  $issue | Add-Member –Type NoteProperty –Name message –Value $entry.message
  If ($entry.relatedLocations.physicalLocation) {
	$issue | Add-Member –Type NoteProperty –Name location –Value (@($entry.locations.resultFile) + $entry.relatedLocations.physicalLocation)
  } Else {
    $issue | Add-Member –Type NoteProperty –Name location –Value $entry.locations.resultFile
  }

  $issue
  return
}

function CreateIssueReports
{
  param ([string]$sarifReportPath)

  $thrd = [Threading.Thread]::CurrentThread
  $thrd.CurrentCulture = [Globalization.CultureInfo]::InvariantCulture
  $thrd.CurrentUICulture = $thrd.CurrentCulture

  # Load the JSON, working around CovertFrom-Json max size limitation
  # See http://stackoverflow.com/questions/16854057/convertfrom-json-max-length
  $contents = Get-Content $sarifReportPath -Raw
  [void][System.Reflection.Assembly]::LoadWithPartialName("System.Web.Extensions")
  $jsonserial= New-Object -TypeName System.Web.Script.Serialization.JavaScriptSerializer
  $jsonserial.MaxJsonLength = 100000000
  $json = $jsonserial.DeserializeObject($contents)

  $pathPrefix = ([System.IO.DirectoryInfo]$pwd.Path).FullName + '\'

  # Is there any issue?
  If ($json.issues) {
    $allIssues = $json.issues

    # Adjust positions to previous SARIF format
    $allIssues.locations.analysisTarget.region |
      Foreach-Object {
        If ($_.startLine -ne $null) {
          $_.startLine = $_.startLine + 1
        }
        If ($_.startColumn -ne $null) {
          $_.startColumn = $_.startColumn + 1
        }
        If ($_.endLine -ne $null) {
          $_.endLine = $_.endLine + 1
        }
        If ($_.endColumn -ne $null) {
          $_.endColumn = $_.endColumn + 1
        }
      }

    FixUriDeclaration($allIssues.locations.analysisTarget)

    $allIssues = $allIssues | Foreach-Object { GetIssue($_) }
  }
  ElseIf ($json.runLogs) {
    $allIssues = $json.runLogs | Foreach-Object {$_.results}

    FixUriDeclaration($allIssues.locations.analysisTarget)

    $allIssues = $allIssues | Foreach-Object { GetIssue($_) }
  }
  ElseIf ($json.runs) {
    $allIssues = $json.runs | Foreach-Object {$_.results}

    FixUriDeclaration($allIssues.locations.resultFile)
	  FixUriDeclaration($allIssues.relatedLocations.physicalLocation)

    $allIssues = $allIssues | Foreach-Object { GetIssueV3($_) }
  }

  # Change spaces to %20
  $allIssues.location |
    Foreach-Object {
	  If ($_.uri) {
	    $_.uri = $_.uri.replace(' ', '%20')
	  }
    }

  # Filter, Sort & Group issues to get a stable SARIF report
  # AD0001's stack traces in the message are unstable
  # CS???? messages are not of interest
  $issuesByRule = $allIssues |
    Where-Object { $_.id -match '^S[0-9]+$' } |                  # Keep SonarAnalyzer rules only
    Sort-Object @{Expression={$_.location.uri}},                 # Regroup same file issues
                @{Expression={$_.location.region.startLine}},    # Sort by source position
                @{Expression={$_.location.region.startColumn}},  # .. idem
                @{Expression={$_.location.region.endLine}},      # .. idem
                @{Expression={$_.location.region.endColumn}},    # .. idem
                @{Expression={$_.message}} |                     # .. and finally by message
    Group-Object @{Expression={$_.id}}                           # Group issues generated by the same rule

  $file = [System.IO.FileInfo]$sarifReportPath
  $file
  $project = ([System.IO.FileInfo]$file.DirectoryName).Name
  $project
  $issuesByRule |
    Foreach-Object {
      $object = New-Object –Type System.Object
      $object | Add-Member –Type NoteProperty –Name issues –Value $_.Group

      $path = Join-Path (Join-Path 'actual' $project) ($file.BaseName + '-' + $_.Name + $file.Extension)
      $path
      $lines =
        (
          (ConvertTo-Json $object -Depth 42 |
				    ForEach-Object { [System.Text.RegularExpressions.Regex]::Unescape($_) }  # Unescape powershell to json automatic escape
			    ) -split "`r`n"												            	                       # Convert JSON to String and split lines
		    ) | Foreach-Object { $_.TrimStart() }                                        # Remove leading spaces
      Set-Content $path $lines
    }
}

# Normalize & overwrite all *.json SARIF files found under the "actual" folder
Get-ChildItem output -filter *.json -recurse |
  Foreach-Object { CreateIssueReports($_.FullName) }