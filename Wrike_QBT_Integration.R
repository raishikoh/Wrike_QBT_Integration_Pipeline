# ==============================================================================
#' Wrike-QBT Integration Pipeline
# ==============================================================================

# -------------------------------------------------------------------------
# Libraries and Configuration
# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(jsonlite)
library(httr)
library(parsedate)
library(lubridate)

# Set working directory
setwd("G:/My Drive/01 Wrike QBT Integration Project")

# Configuration
CONFIG <- list(
  # Input file path
  input_file = "INPUT_FILE_NAME.csv",
  
  # Target space and project configuration
  target_space = "QBT-Wrike Integration",
  target_project_number = "00-01",  # Focus on Sample Project 1
  target_project_name = "00-01 Sample Project 1",
  
  # Week range for this run (updated to match filename)
  week_start = as.Date("2025-08-03"),
  week_end = as.Date("2025-08-09"),
  
  # API rate limiting
  api_delay = 0.7  # seconds between requests
)

# -------------------------------------------------------------------------
# Wrike Authentication
# -------------------------------------------------------------------------

# Your Wrike API credentials
wrike_token = "ENTER_TOKEN"
access_token = paste0("Bearer ", wrike_token)
base_url = "https://www.wrike.com/api/v4"

# Test connection
cat("🔄 Testing Wrike API connection...\n")
test_connection = GET(paste0(base_url, "/contacts"),
                      add_headers("Authorization" = access_token))

if(test_connection$status_code == 200) {
  cat("✅ Wrike API connection successful!\n")
} else {
  cat("❌ Connection failed - Status:", test_connection$status_code, "\n")
  stop("API connection failed")
}

# -------------------------------------------------------------------------
# Helper Functions
# -------------------------------------------------------------------------

#' Extract project number from task/project name (vectorized)
extract_project_number <- function(names) {
  # Handle NA and NULL values
  result <- rep(NA_character_, length(names))
  valid_indices <- !is.na(names) & !is.null(names)
  
  if(any(valid_indices)) {
    pattern <- "^(\\d{2}-\\d{2})"
    matches <- regmatches(names[valid_indices], regexpr(pattern, names[valid_indices]))
    
    # Extract the first group (the project number) from each match
    for(i in seq_along(matches)) {
      if(length(matches[[i]]) > 0) {
        result[which(valid_indices)[i]] <- matches[[i]][1]
      }
    }
  }
  
  return(result)
}

#' Generate unique time entry identifier for duplicate detection
generate_time_entry_id <- function(user_id, task_id, date, hours) {
  paste(user_id, task_id, date, hours, sep = "_")
}

#' Safe API request with error handling
safe_api_request <- function(method, url, body = NULL) {
  tryCatch({
    if(method == "GET") {
      response <- GET(url, add_headers("Authorization" = access_token))
    } else if(method == "POST") {
      response <- POST(url, 
                       add_headers("Authorization" = access_token,
                                   "Content-Type" = "application/json"),
                       body = body, encode = "json")
    } else {
      stop("Unsupported method")
    }
    
    Sys.sleep(CONFIG$api_delay)  # Rate limiting
    return(response)
  }, error = function(e) {
    cat("❌ API request failed:", e$message, "\n")
    return(NULL)
  })
}

# -------------------------------------------------------------------------
# Data Loading and Preprocessing
# -------------------------------------------------------------------------

cat("📁 Loading timesheet data...\n")

# Check if input file exists
if(!file.exists(CONFIG$input_file)) {
  stop(paste("Input file not found:", CONFIG$input_file))
}

# Load and preprocess timesheet data
time_raw <- tryCatch({
  read.csv(CONFIG$input_file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}, error = function(e) {
  cat("❌ Error reading CSV file:", e$message, "\n")
  stop("Failed to read input file")
})

cat("📊 Raw data loaded:", nrow(time_raw), "records\n")

# Clean and prepare the data
time_processed <- time_raw %>%
  # Ensure required columns exist
  {if(!"jobcode_1" %in% names(.)) stop("jobcode_1 column not found"); .} %>%
  {if(!"jobcode_2" %in% names(.)) stop("jobcode_2 column not found"); .} %>%
  {if(!"hours" %in% names(.)) stop("hours column not found"); .} %>%
  {if(!"local_date" %in% names(.)) stop("local_date column not found"); .} %>%
  {if(!"fname" %in% names(.)) stop("fname column not found"); .} %>%
  {if(!"lname" %in% names(.)) stop("lname column not found"); .} %>%
  # Create combined job code and full name
  mutate(
    job_code = ifelse(!is.na(jobcode_2), 
                      paste(jobcode_1, jobcode_2, sep = " >> "), 
                      jobcode_1),
    full_name = paste(fname, lname),
    local_date = as.Date(local_date),
    project_number = extract_project_number(job_code)
  ) %>%
  # Filter for target project and date range
  filter(
    !is.na(project_number),
    project_number == CONFIG$target_project_number,
    local_date >= CONFIG$week_start,
    local_date <= CONFIG$week_end,
    !is.na(hours),
    hours > 0
  ) %>%
  # Parse project and task from job_code
  separate(job_code, into = c("project_name", "task_name"), 
           sep = " >> ", remove = FALSE, fill = "right") %>%
  # Clean up task names (remove project number prefix if present)
  mutate(
    task_name = ifelse(is.na(task_name), project_name, task_name),
    task_name_clean = gsub("^\\d{2}-\\d{2}\\s*", "", task_name)
  )

cat("📋 Processed data:", nrow(time_processed), "relevant records for project", CONFIG$target_project_number, "\n")

if(nrow(time_processed) == 0) {
  cat("⚠️  No time entries found for project", CONFIG$target_project_number, "in the specified date range\n")
  
  # Show available project numbers for debugging
  available_projects <- time_raw %>%
    mutate(
      job_code = ifelse(!is.na(jobcode_2), 
                        paste(jobcode_1, jobcode_2, sep = " >> "), 
                        jobcode_1),
      project_number = extract_project_number(job_code)
    ) %>%
    filter(!is.na(project_number)) %>%
    distinct(project_number, job_code) %>%
    arrange(project_number)
  
  cat("Available projects in data:\n")
  print(available_projects)
  stop("No relevant data to process")
}

# -------------------------------------------------------------------------
# Wrike Structure Discovery
# -------------------------------------------------------------------------

cat("🔍 Discovering Wrike structure...\n")

# Get spaces
spaces_response <- GET(paste0(base_url, "/spaces"), 
                       add_headers("Authorization" = access_token))

if(spaces_response$status_code != 200) {
  cat("❌ Failed to fetch spaces. Status code:", spaces_response$status_code, "\n")
  stop("Failed to fetch spaces")
}

spaces_content <- content(spaces_response, "parsed")

# Find target space
target_space <- NULL
for(i in 1:length(spaces_content$data)) {
  space <- spaces_content$data[[i]]
  if(space$title == CONFIG$target_space) {
    target_space <- space
    break
  }
}

if(is.null(target_space)) {
  cat("❌ Target space '", CONFIG$target_space, "' not found\n")
  cat("Available spaces:\n")
  for(i in 1:length(spaces_content$data)) {
    cat("-", spaces_content$data[[i]]$title, "\n")
  }
  stop("Target space not found")
}

cat("✅ Found target space:", target_space$title, "| ID:", target_space$id, "\n")

# Get folders/projects in the space
folders_url <- paste0(base_url, "/spaces/", target_space$id, "/folders")
folders_response <- GET(folders_url, add_headers("Authorization" = access_token))

if(folders_response$status_code != 200) {
  cat("❌ Failed to fetch folders in space. Status code:", folders_response$status_code, "\n")
  stop("Failed to fetch folders in space")
}

folders_content <- content(folders_response, "parsed")

# Find target project
target_project <- NULL
for(i in 1:length(folders_content$data)) {
  folder <- folders_content$data[[i]]
  if(grepl(CONFIG$target_project_number, folder$title, fixed = TRUE)) {
    target_project <- folder
    break
  }
}

if(is.null(target_project)) {
  cat("❌ Target project with number '", CONFIG$target_project_number, "' not found\n")
  cat("Available folders/projects:\n")
  for(i in 1:length(folders_content$data)) {
    cat("-", folders_content$data[[i]]$title, "\n")
  }
  stop("Target project not found")
}

cat("✅ Found target project:", target_project$title, "| ID:", target_project$id, "\n")

# Get tasks in the project
tasks_url <- paste0(base_url, "/folders/", target_project$id, "/tasks")
tasks_response <- GET(tasks_url, add_headers("Authorization" = access_token))

if(tasks_response$status_code != 200) {
  cat("❌ Failed to fetch tasks in project. Status code:", tasks_response$status_code, "\n")
  stop("Failed to fetch tasks in project")
}

tasks_content <- content(tasks_response, "parsed")

cat("📋 Found", length(tasks_content$data), "tasks in project\n")

# Build task mapping
task_mapping <- data.frame(
  task_id = character(),
  task_title = character(),
  task_name_clean = character(),
  project_id = character(),
  stringsAsFactors = FALSE
)

# Check if there are any tasks
if(length(tasks_content$data) > 0) {
  for(i in 1:length(tasks_content$data)) {
    task <- tasks_content$data[[i]]
    # Extract clean task name (remove project number prefix)
    clean_name <- gsub("^\\d{2}-\\d{2}\\s*", "", task$title)
    
    task_mapping <- rbind(task_mapping, data.frame(
      task_id = task$id,
      task_title = task$title,
      task_name_clean = clean_name,
      project_id = target_project$id,
      stringsAsFactors = FALSE
    ))
  }
  
  cat("Tasks found:\n")
  print(task_mapping)
} else {
  cat("⚠️  No tasks found in the project. This might be because:\n")
  cat("   1. The project has no tasks yet\n")
  cat("   2. Tasks are in subfolders/subprojects\n")
  cat("   3. Access permissions don't allow viewing tasks\n")
  
  # Try to get tasks recursively by checking subfolders
  cat("🔍 Checking for subfolders in the project...\n")
  
  subfolders_url <- paste0(base_url, "/folders/", target_project$id, "/folders")
  subfolders_response <- GET(subfolders_url, add_headers("Authorization" = access_token))
  
  if(subfolders_response$status_code == 200) {
    subfolders_content <- content(subfolders_response, "parsed")
    cat("📁 Found", length(subfolders_content$data), "subfolders\n")
    
    if(length(subfolders_content$data) > 0) {
      cat("Subfolders in project:\n")
      for(i in 1:length(subfolders_content$data)) {
        subfolder <- subfolders_content$data[[i]]
        cat("-", subfolder$title, "| ID:", subfolder$id, "\n")
        
        # Get tasks in each subfolder
        subfolder_tasks_url <- paste0(base_url, "/folders/", subfolder$id, "/tasks")
        subfolder_tasks_response <- GET(subfolder_tasks_url, add_headers("Authorization" = access_token))
        
        if(subfolder_tasks_response$status_code == 200) {
          subfolder_tasks_content <- content(subfolder_tasks_response, "parsed")
          cat("  └─ Tasks in this subfolder:", length(subfolder_tasks_content$data), "\n")
          
          # Add tasks from subfolders to our mapping
          if(length(subfolder_tasks_content$data) > 0) {
            for(j in 1:length(subfolder_tasks_content$data)) {
              task <- subfolder_tasks_content$data[[j]]
              clean_name <- gsub("^\\d{2}-\\d{2}\\s*", "", task$title)
              
              task_mapping <- rbind(task_mapping, data.frame(
                task_id = task$id,
                task_title = task$title,
                task_name_clean = clean_name,
                project_id = subfolder$id,  # Use subfolder ID as project ID
                stringsAsFactors = FALSE
              ))
              cat("     -", task$title, "\n")
            }
          }
        }
      }
    }
  }
}

# Display final task mapping
if(nrow(task_mapping) > 0) {
  cat("\n✅ Final task mapping:\n")
  print(task_mapping)
} else {
  cat("\n❌ No tasks found in project or its subfolders\n")
  stop("Cannot proceed without tasks to map time entries to")
}

# -------------------------------------------------------------------------
# User Mapping
# -------------------------------------------------------------------------

cat("👥 Setting up user mapping...\n")

# Get contacts
contacts_response <- GET(paste0(base_url, "/contacts"), 
                         add_headers("Authorization" = access_token))

if(contacts_response$status_code != 200) {
  cat("❌ Failed to fetch contacts. Status code:", contacts_response$status_code, "\n")
  stop("Failed to fetch contacts")
}

contacts_content <- content(contacts_response, "parsed")

# Build user mapping
user_mapping <- data.frame(
  full_name = character(),
  user_id = character(),
  email = character(),
  stringsAsFactors = FALSE
)

for(i in 1:length(contacts_content$data)) {
  contact <- contacts_content$data[[i]]
  if(!is.null(contact$firstName) && !is.null(contact$lastName)) {
    full_name <- paste(contact$firstName, contact$lastName)
    email <- ifelse(is.null(contact$primaryEmail), "", contact$primaryEmail)
    
    user_mapping <- rbind(user_mapping, data.frame(
      full_name = full_name,
      user_id = contact$id,
      email = email,
      stringsAsFactors = FALSE
    ))
  }
}

cat("Available users:\n")
print(user_mapping)

# -------------------------------------------------------------------------
# Data Mapping and Validation
# -------------------------------------------------------------------------

cat("🔗 Mapping timesheet data to Wrike structure...\n")

# Create custom task mapping for your actual timesheet tasks
# This allows mapping between QuickBooks Time task names and Wrike task names
custom_task_mapping <- data.frame(
  timesheet_task_name = c(
    "EGIS HR",
    "_Project_Management", 
    "Project Management",
    "120-123 SKT FINANCE: Funding Diversification (Time)",
    "EGIS Admin"
  ),
  wrike_task_name = c(
    "Sample Project 1-1-1",  # Map EGIS HR to first task
    "Sample Project 1-1-2",  # Map Project Management variations to second task
    "Sample Project 1-1-2",  # Map another Project Management to same task
    "Sample Project 1-2-1",  # Map SKT FINANCE to different task
    "Sample Project 1-3-1"   # Map EGIS Admin to another task
  ),
  stringsAsFactors = FALSE
)

cat("📋 Custom task mapping:\n")
print(custom_task_mapping)

# Map time entries to Wrike tasks and users using custom mapping
time_for_wrike <- time_processed %>%
  # Join with user mapping
  left_join(user_mapping, by = "full_name") %>%
  # Apply custom task mapping first
  left_join(custom_task_mapping, by = c("task_name_clean" = "timesheet_task_name")) %>%
  # Join with Wrike task mapping using the custom mapping
  left_join(task_mapping, by = c("wrike_task_name" = "task_name_clean")) %>%
  # Create unique entry ID for duplicate detection
  mutate(
    entry_id = generate_time_entry_id(user_id, task_id, local_date, hours),
    notes = paste("Migrated from QBT for week", CONFIG$week_start, "to", CONFIG$week_end),
    # Keep original task name for reference
    original_task_name = task_name_clean,
    # Use mapped task name
    task_name_clean = ifelse(!is.na(wrike_task_name), wrike_task_name, task_name_clean)
  ) %>%
  # Select final columns
  select(full_name, user_id, original_task_name, task_name_clean, task_id, task_title, 
         local_date, hours, notes, entry_id) %>%
  # Remove entries with missing mappings
  filter(!is.na(user_id), !is.na(task_id))

cat("📊 Mapped time entries:\n")
print(time_for_wrike)

# Check for mapping issues
missing_users <- time_processed %>%
  anti_join(user_mapping, by = "full_name") %>%
  distinct(full_name)

missing_tasks_after_mapping <- time_processed %>%
  left_join(custom_task_mapping, by = c("task_name_clean" = "timesheet_task_name")) %>%
  filter(is.na(wrike_task_name)) %>%
  distinct(task_name_clean, task_name)

if(nrow(missing_users) > 0) {
  cat("⚠️  Users not found in Wrike:\n")
  print(missing_users)
}

if(nrow(missing_tasks_after_mapping) > 0) {
  cat("⚠️  Tasks still not mapped after custom mapping:\n")
  print(missing_tasks_after_mapping)
  cat("💡 Add these to the custom_task_mapping section if needed\n")
}

# Display detailed comparison for troubleshooting
cat("\n🔍 DETAILED MAPPING ANALYSIS:\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n📋 Original timesheet tasks vs Mapped Wrike tasks:\n")
mapping_summary <- time_processed %>%
  left_join(custom_task_mapping, by = c("task_name_clean" = "timesheet_task_name")) %>%
  distinct(task_name_clean, wrike_task_name) %>%
  arrange(task_name_clean)
print(mapping_summary)

cat("\n📊 SUMMARY:\n")
cat("- Total timesheet entries:", nrow(time_processed), "\n")
cat("- Successfully mapped entries:", nrow(time_for_wrike), "\n")
cat("- Missing users:", nrow(missing_users), "\n")
cat("- Unmapped tasks:", nrow(missing_tasks_after_mapping), "\n")

if(nrow(time_for_wrike) == 0) {
  # Export analysis files for debugging
  analysis_filename <- paste0("mapping_analysis_", 
                              format(CONFIG$week_start, "%Y%m%d"), "_", 
                              format(CONFIG$week_end, "%Y%m%d"), ".csv")
  
  # Export detailed analysis
  write.csv(time_processed, paste0("timesheet_data_", analysis_filename), row.names = FALSE)
  write.csv(task_mapping, paste0("wrike_tasks_", analysis_filename), row.names = FALSE)
  write.csv(custom_task_mapping, paste0("custom_mapping_", analysis_filename), row.names = FALSE)
  
  cat("\n📄 Analysis files exported for debugging:\n")
  cat("- timesheet_data_", analysis_filename, "\n")
  cat("- wrike_tasks_", analysis_filename, "\n") 
  cat("- custom_mapping_", analysis_filename, "\n")
  
  cat("\n❌ SCRIPT STOPPED: No time entries could be mapped\n")
  cat("🔧 TO FIX: Update the custom_task_mapping section in the script\n")
  stop("Task mapping failed - check custom mapping configuration")
}

# -------------------------------------------------------------------------
# Duplicate Detection
# -------------------------------------------------------------------------

cat("🛡️  Checking for existing time entries to prevent duplicates...\n")

# Get existing time logs for this week and project
existing_timelogs <- data.frame(
  task_id = character(),
  user_id = character(),
  tracked_date = as.Date(character()),
  hours = numeric(),
  entry_id = character(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(task_mapping)) {
  task_id <- task_mapping$task_id[i]
  
  # Get timelogs for this task in the date range
  timelogs_url <- paste0(base_url, "/tasks/", task_id, "/timelogs")
  timelogs_params <- paste0("?trackedDate=", CONFIG$week_start, ",", CONFIG$week_end)
  
  timelogs_response <- GET(paste0(timelogs_url, timelogs_params),
                           add_headers("Authorization" = access_token))
  
  if(timelogs_response$status_code == 200) {
    timelogs_content <- content(timelogs_response, "parsed")
    
    if(length(timelogs_content$data) > 0) {
      for(j in 1:length(timelogs_content$data)) {
        timelog <- timelogs_content$data[[j]]
        
        # Safely extract values with NULL checks
        userid <- if(is.null(timelog$userid) || length(timelog$userid) == 0) NA_character_ else timelog$userid
        tracked_date <- if(is.null(timelog$trackedDate) || length(timelog$trackedDate) == 0) NA_character_ else timelog$trackedDate
        hours <- if(is.null(timelog$hours) || length(timelog$hours) == 0) 0 else timelog$hours
        
        # Only add if we have valid data
        if(!is.na(userid) && !is.na(tracked_date)) {
          entry_id <- generate_time_entry_id(userid, task_id, as.Date(tracked_date), hours)
          
          existing_timelogs <- rbind(existing_timelogs, data.frame(
            task_id = task_id,
            user_id = userid,
            tracked_date = as.Date(tracked_date),
            hours = hours,
            entry_id = entry_id,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Add small delay between API calls
  Sys.sleep(0.1)
}

cat("📋 Found", nrow(existing_timelogs), "existing time entries for this week\n")

# Filter out duplicates
if(nrow(existing_timelogs) > 0) {
  time_for_upload <- time_for_wrike %>%
    anti_join(existing_timelogs, by = "entry_id")
  
  duplicates_found <- nrow(time_for_wrike) - nrow(time_for_upload)
  if(duplicates_found > 0) {
    cat("⚠️ ", duplicates_found, "duplicate entries detected and skipped\n")
  }
} else {
  time_for_upload <- time_for_wrike
}

cat("📊 Time entries ready for upload:", nrow(time_for_upload), "\n")

if(nrow(time_for_upload) == 0) {
  cat("✅ All time entries already exist in Wrike. No uploads needed.\n")
  quit(save = "no", status = 0)
}

# -------------------------------------------------------------------------
# Time Entry Upload
# -------------------------------------------------------------------------

cat("⬆️  Starting time entry upload to Wrike...\n")

# Initialize counters
upload_results <- data.frame(
  full_name = character(),
  task_title = character(),
  date = character(),
  hours = numeric(),
  status = character(),
  message = character(),
  stringsAsFactors = FALSE
)

successful_posts <- 0
failed_posts <- 0

for(i in 1:nrow(time_for_upload)) {
  entry <- time_for_upload[i, ]
  
  # Prepare time entry data for Wrike API
  time_entry_data <- list(
    hours = entry$hours,
    trackedDate = format(entry$local_date, "%Y-%m-%d"),
    comment = entry$notes
  )
  
  # POST to Wrike
  post_url <- paste0(base_url, "/tasks/", entry$task_id, "/timelogs")
  
  response <- POST(post_url,
                   add_headers("Authorization" = access_token,
                               "Content-Type" = "application/json"),
                   body = time_entry_data, encode = "json")
  
  # Add rate limiting
  Sys.sleep(CONFIG$api_delay)
  
  # Process response
  if(response$status_code == 201) {
    successful_posts <- successful_posts + 1
    status_msg <- "✅ SUCCESS"
    result_msg <- "Time entry created successfully"
    cat("✅", entry$full_name, "|", entry$task_title, "|", 
        entry$local_date, "|", entry$hours, "hrs\n")
  } else {
    failed_posts <- failed_posts + 1
    status_msg <- "❌ FAILED"
    result_msg <- paste("HTTP", response$status_code)
    cat("❌", entry$full_name, "|", entry$task_title, "|", 
        entry$local_date, "|", entry$hours, "hrs | Error:", result_msg, "\n")
  }
  
  # Record result
  upload_results <- rbind(upload_results, data.frame(
    full_name = entry$full_name,
    task_title = entry$task_title,
    date = format(entry$local_date, "%Y-%m-%d"),
    hours = entry$hours,
    status = status_msg,
    message = result_msg,
    stringsAsFactors = FALSE
  ))
}

# -------------------------------------------------------------------------
# Results Summary and Reporting
# -------------------------------------------------------------------------

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("📊 MIGRATION RESULTS SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("📅 Week processed:", format(CONFIG$week_start, "%Y-%m-%d"), "to", 
    format(CONFIG$week_end, "%Y-%m-%d"), "\n")
cat("🎯 Target project:", CONFIG$target_project_name, "\n")
cat("📈 Successful uploads:", successful_posts, "\n")
cat("❌ Failed uploads:", failed_posts, "\n")
cat("📋 Total entries processed:", nrow(time_for_upload), "\n")

if(failed_posts == 0) {
  cat("🎉 SUCCESS! All time entries uploaded to Wrike\n")
} else {
  cat("⚠️  Some uploads failed - check detailed results below\n")
}

# Export detailed results
results_filename <- paste0("migration_results_", 
                           format(CONFIG$week_start, "%Y%m%d"), "_", 
                           format(CONFIG$week_end, "%Y%m%d"), "_",
                           gsub("[^A-Za-z0-9]", "", CONFIG$target_project_number),
                           ".csv")

write.csv(upload_results, results_filename, row.names = FALSE)
cat("📄 Detailed results exported to:", results_filename, "\n")

# Export processed data for reference
data_filename <- paste0("processed_timesheet_", 
                        format(CONFIG$week_start, "%Y%m%d"), "_", 
                        format(CONFIG$week_end, "%Y%m%d"), "_",
                        gsub("[^A-Za-z0-9]", "", CONFIG$target_project_number),
                        ".csv")

write.csv(time_for_upload, data_filename, row.names = FALSE)
cat("📄 Processed timesheet data exported to:", data_filename, "\n")

cat("\n✅ Script execution completed!\n")

# Final status
if(failed_posts > 0) {
  stop("Script completed with errors")
} else {
  cat("🎉 All operations completed successfully!\n")
}