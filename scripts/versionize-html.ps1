param (
  [string]$html,
  [string]$version = (Get-Date -format "yyyyMMdd_HHmmss")
)

Write-Output "Versionizing $($html) as $($version)"

(Get-Content $html) `
    -replace '(?<="[^:"]+\.(?:js|css))(?=")', "?Rel=$($version)" `
    |
  Out-File $html -Force -Encoding ASCII
