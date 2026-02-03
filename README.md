# Wrike-QBT Integration

This repository contains an R-based integration tool for synchronizing and analyzing timesheet data between Wrike project management platform and QuickBooks Time (formerly TSheets). The script automates timesheet reconciliation, identifies discrepancies, and generates comprehensive reports.

## Installation Guidelines

**Required R Packages:**<br/>
```r
install.packages(c("httr", "jsonlite", "dplyr", "lubridate", "ggplot2", "plotly"))
```

**API Access Requirements:**<br/>
- Wrike API Token (obtain from Wrike account settings)
- QuickBooks Time credentials: Client ID, Client Secret, Access Token

**Setup Instructions:**<br/>
1. Clone or download this repository to your local machine<br/>
2. Install the required R packages using the command above<br/>
3. Create a `.Renviron` file in your project directory for secure credential storage<br/>
4. Configure API credentials and date range parameters as needed<br/>

## Script Overview

### Wrike-QBT Integration Script

This script provides automated timesheet reconciliation between Wrike and QuickBooks Time platforms, including:

- **API Integration**: Connects to both Wrike and QuickBooks Time REST APIs
- **Data Retrieval**: Fetches timelog and timesheet data for specified date ranges
- **Data Processing**: Normalizes and structures data from both platforms for comparison
- **Mismatch Detection**: Identifies discrepancies between platform entries
- **Visualization**: Generates interactive charts for time tracking analysis
- **Export Functionality**: Saves reconciliation reports and summary statistics

**What the Script Can Detect:**
- Missing time entries in either platform
- Time duration discrepancies between platforms
- Duplicate or conflicting time entries
- Data inconsistencies (date, user, project mismatches)
- API connection or authentication issues
- Rate limiting or timeout errors
- Invalid date formats or data structures

**What the Script Can Generate:**
- **Reconciliation Report**: CSV file with detailed comparison of time entries
- **Summary Statistics**: Text file with aggregate metrics and totals
- **Interactive Visualizations**: Plotly charts showing:
  - Time entries by project (bar chart)
  - Daily time tracking trends (line chart)
  - Platform comparison (grouped bar chart)
  - User activity reports (scatter plot)
- **Discrepancy Logs**: Detailed mismatch information with specific entry IDs

**Key Features:**
- Robust error handling for API calls
- Flexible date range configuration
- Automatic data validation and filtering
- User-friendly console output with progress updates
- Support for multiple projects and users
- Customizable reconciliation thresholds

## Usage Instructions

### Basic Workflow

1. **Configure Credentials:**
   ```r
   # Create .Renviron file with:
   WRIKE_API_TOKEN=your_wrike_token_here
   QBT_CLIENT_ID=your_client_id
   ```

2. **Set Date Range:**
   ```r
   # Default: Previous month
   start_date <- floor_date(today() - months(1), "month")
   end_date <- ceiling_date(start_date, "month") - days(1)
   
   # Or customize:
   start_date <- as.Date("2024-01-01")
   end_date <- as.Date("2024-01-31")
   ```

3. **Run the Script:**
   ```r
   source("Wrike_QBT_Integration_v14.R")
   ```

4. **Review Output:**
   - Check console for processing status and error messages
   - Review generated CSV files for detailed reconciliation data
   - Open HTML visualizations in browser for interactive analysis

### Configuration Options

**Key Variables:**
- `wrike_token`: Wrike API authentication token
- `client_id`, `client_secret`, `access_token`: QuickBooks Time OAuth credentials
- `start_date`, `end_date`: Date range for data retrieval
- `output_directory`: Location for saving reports (default: current directory)

**Processing Options:**
- `validate_data`: Enable/disable automatic data validation
- `filter_zero_duration`: Remove entries with zero duration
- `aggregate_by_project`: Group results by project or user
- `export_format`: Choose between CSV, Excel, or JSON output

## Additional Sections

### Features
- **Dual Platform Integration**: Seamless connection to Wrike and QuickBooks Time APIs
- **Automated Reconciliation**: Intelligent matching algorithm for time entries
- **Comprehensive Error Detection**: Identifies missing, duplicate, or conflicting entries
- **Interactive Visualizations**: Plotly-powered charts for data exploration
- **Flexible Date Filtering**: Supports custom date ranges and time periods
- **Detailed Reporting**: CSV exports with feature-level discrepancy details
- **Secure Credential Management**: Environment variable support for API keys

### Special Instructions
- **API Rate Limits**: QuickBooks Time API has a limit of 300 requests per minute. For large date ranges, the script implements automatic delays
- **Date Format**: All dates must be in ISO format (YYYY-MM-DD)
- **Authentication**: Ensure API tokens are current and not expired
- **File Naming**: Output files are timestamped to prevent overwriting previous reports
- **Large Datasets**: For organizations with extensive time tracking, consider processing data in smaller date ranges
- **Security Best Practices**: Never commit `.Renviron` file or hard-coded credentials to version control

### Output Files
The script generates the following files:

**`timesheet_comparison_YYYYMMDD.csv`**
- Detailed reconciliation report with row-by-row comparisons
- Columns: Date, User, Project, Wrike_Hours, QBT_Hours, Difference, Status

**`summary_statistics_YYYYMMDD.txt`**
- Total hours tracked in each platform
- Number of matched entries
- Number and percentage of discrepancies
- Project-level and user-level summaries

### Troubleshooting Guide

**Common Issues:**

1. **Authentication Error (401 Unauthorized)**
   - Verify API tokens are current and valid
   - Check that credentials are properly loaded from `.Renviron`
   - Confirm API access is enabled in platform settings

2. **Rate Limiting (429 Too Many Requests)**
   - Reduce date range to retrieve fewer records
   - Implement delays between API calls (built into script)
   - Consider breaking large requests into smaller batches

3. **No Data Retrieved**
   - Verify date range contains actual time entries
   - Check that users have logged time in both platforms
   - Confirm date format is correct (YYYY-MM-DD)

4. **Mismatch Detection Issues**
   - Review matching criteria (date, user, project)
   - Check for differences in user names or project titles between platforms
   - Verify time zones are consistent across platforms

5. **Package Installation Errors**
   - Update R to latest version (4.0.0 or higher recommended)
   - Install packages individually if bulk install fails
   - Check for package dependencies

### API Endpoint Information

**Wrike API:**
- Base URL: `https://www.wrike.com/api/v4/`
- Timelogs Endpoint: `/timelogs`
- Authentication: Bearer token
- Documentation: https://developers.wrike.com/

**QuickBooks Time API:**
- Base URL: `https://rest.tsheets.com/api/v1/`
- Timesheets Endpoint: `/timesheets`
- Authentication: OAuth 2.0
- Documentation: https://developers.tsheets.com/

## Project Information

- **Developed by**: Ali Sehpar Shikoh (Eclipse Geomatics Ltd.)
- **Version**: 1
- **Last Updated**: February 2026

---

**Security Note**: Keep your API credentials secure and never commit them to version control. Always use environment variables or secure credential management systems.
