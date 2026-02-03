# ==============================================================================
#' Wrike-QBT Integration Pipeline
# ==============================================================================

# -------------------------------------------------------------------------
# Libraries, dirs, input data
# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)  # Added for separate() function
library(jsonlite)
library(httr)
library(parsedate)

# set working directory
setwd("C:/Users/ali/Downloads")

# input data
report_name = "INPUT_FILE_NAME.csv"
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Wrike Authentication
# -------------------------------------------------------------------------

# Your Wrike API credentials
wrike_token = "ENTER_TOKEN"
access_token = paste0("Bearer ", wrike_token)

# Wrike API base URL
base_url = "https://www.wrike.com/api/v4"

# Test connection
test_connection = GET(paste0(base_url, "/contacts"),
                      add_headers("Authorization" = access_token))

if(test_connection$status_code == 200) {
  print("✓ Wrike API connection successful!")
} else {
  print("✗ Connection failed - check your token")
  stop()
}

# -------------------------------------------------------------------------
# Prep QuickBooks Time Export
# -------------------------------------------------------------------------

# Import CSV from QuickBooks Time - this format has a title row, so skip it
time_raw = read.csv(report_name, skip = 1, na.strings = c("","NA")) %>%
  # Split the job_code column into project and task
  separate(job_code, into = c("project_name", "task_name"), sep = " >> ", remove = FALSE) %>%
  # Filter for test project only
  filter(., project_name == "QBT-Wrike Integration Testing")

print(paste("Found", nrow(time_raw), "time entries for test project"))

if(nrow(time_raw) == 0) {
  print("No time entries found for 'QBT-Wrike Integration Testing' project")
  # Read file again to show available projects
  all_data = read.csv(report_name, skip = 1, na.strings = c("","NA")) %>%
    separate(job_code, into = c("project_name", "task_name"), sep = " >> ", remove = FALSE)
  print("Available projects in your export:")
  print(unique(all_data$project_name))
  stop()
}

# Prepare data for Wrike
hours_summary = time_raw %>%
  select(., project_name, task_name, hours) %>%
  # Add current user and date since this export doesn't include them
  mutate(., 
         name = "Current User",  # We'll need to identify the actual user
         local_date = Sys.Date(),  # Use today's date
         combined_notes = "Migrated from QuickBooks Time"
  )

print("Time summary prepared:")
print(hours_summary)

# -------------------------------------------------------------------------
# Get Wrike Structure - Projects, Tasks, Users
# -------------------------------------------------------------------------

# Get all folders (projects)
folders = GET(paste0(base_url, "/folders"),
              add_headers("Authorization" = access_token))
folders_content = content(folders, "parsed")

# Find the test project folder
test_project = NULL
for(i in 1:length(folders_content$data)) {
  folder = folders_content$data[[i]]
  if(folder$title == "QBT-Wrike Integration Testing") {
    test_project = folder
    break
  }
}

if(is.null(test_project)) {
  print("ERROR: Could not find 'QBT-Wrike Integration Testing' project in Wrike")
  print("Available projects:")
  for(i in 1:length(folders_content$data)) {
    print(folders_content$data[[i]]$title)
  }
  stop()
}

print(paste("✓ Found test project:", test_project$title, "| ID:", test_project$id))

# Get tasks for the test project
tasks_url = paste0(base_url, "/folders/", test_project$id, "/tasks")
tasks = GET(tasks_url, add_headers("Authorization" = access_token))
tasks_content = content(tasks, "parsed")

print(paste("Found", length(tasks_content$data), "tasks in test project:"))
task_mapping = data.frame()

for(i in 1:length(tasks_content$data)) {
  task = tasks_content$data[[i]]
  print(paste("- Task:", task$title, "| ID:", task$id))
  
  # Build task mapping dataframe
  task_mapping = rbind(task_mapping, data.frame(
    task_name = task$title,
    task_id = task$id,
    project_name = "QBT-Wrike Integration Testing",
    project_id = test_project$id
  ))
}

# Get users/contacts
contacts = GET(paste0(base_url, "/contacts"),
               add_headers("Authorization" = access_token))
contacts_content = content(contacts, "parsed")

# Build user mapping
user_mapping = data.frame()
for(i in 1:length(contacts_content$data)) {
  contact = contacts_content$data[[i]]
  if(!is.null(contact$firstName) && !is.null(contact$lastName)) {
    full_name = paste(contact$firstName, contact$lastName)
    user_mapping = rbind(user_mapping, data.frame(
      name = full_name,
      user_id = contact$id,
      email = ifelse(is.null(contact$primaryEmail), "", contact$primaryEmail)
    ))
  }
}

print("Available users:")
print(user_mapping)

# -------------------------------------------------------------------------
# Map QuickBooks Time Data to Wrike Structure
# -------------------------------------------------------------------------

# Since this export doesn't include user information, we'll need to determine the user
# For testing, we'll use the first user that matches the current R user or a default
current_user_name = "Ali Sehpar Shikoh"  # Updated to your name

# Find the current user in Wrike
current_user = user_mapping[user_mapping$name == current_user_name, ]
if(nrow(current_user) == 0) {
  print(paste("ERROR: Could not find user '", current_user_name, "' in Wrike"))
  print("Available users:")
  print(user_mapping$name)
  stop()
}

# If multiple users with same name, use the first one (work email)
if(nrow(current_user) > 1) {
  print("Multiple users found with same name, using first one:")
  print(current_user)
  current_user = current_user[1, ]
}

print(paste("Using user:", current_user$name, "| ID:", current_user$user_id, "| Email:", current_user$email))

# Prepare time entries for Wrike
time_for_wrike = hours_summary %>%
  # Add the current user info
  mutate(., 
         name = current_user_name,
         user_id = current_user$user_id
  ) %>%
  # Join with task mapping  
  left_join(., task_mapping, by = c("project_name", "task_name")) %>%
  # Convert date format for Wrike (YYYY-MM-DD)
  mutate(., wrike_date = as.Date(local_date)) %>%
  # Rename hours column
  rename(., sum_hours = hours) %>%
  # Select final columns
  select(., name, user_id, task_name, task_id, wrike_date, sum_hours, combined_notes)

print("Mapped time entries for Wrike:")
print(time_for_wrike)

# Check for mapping issues
missing_users = time_for_wrike[is.na(time_for_wrike$user_id), ]
missing_tasks = time_for_wrike[is.na(time_for_wrike$task_id), ]

if(nrow(missing_users) > 0) {
  print("ERROR: Could not find users in Wrike:")
  print(unique(missing_users$name))
  stop()
}

if(nrow(missing_tasks) > 0) {
  print("ERROR: Could not find tasks in Wrike:")
  print(unique(missing_tasks$task_name))
  print("Available tasks were:", task_mapping$task_name)
  stop()
}

# -------------------------------------------------------------------------
# POST Time Entries to Wrike
# -------------------------------------------------------------------------

print("Starting time entry upload to Wrike...")

successful_posts = 0
failed_posts = 0

for(i in 1:nrow(time_for_wrike)) {
  entry = time_for_wrike[i, ]
  
  # Prepare time entry data for Wrike API
  time_entry_data = list(
    hours = entry$sum_hours,
    trackedDate = format(entry$wrike_date, "%Y-%m-%d"),
    comment = ifelse(is.na(entry$combined_notes), "", entry$combined_notes)
  )
  
  # POST to Wrike
  post_url = paste0(base_url, "/tasks/", entry$task_id, "/timelogs")
  
  tryCatch({
    response = POST(
      url = post_url,
      add_headers(
        "Authorization" = access_token,
        "Content-Type" = "application/json"
      ),
      body = time_entry_data,
      encode = "json"
    )
    
    if(response$status_code == 201) {
      successful_posts = successful_posts + 1
      print(paste("✓ Posted:", entry$name, "|", entry$task_name, "|", entry$sum_hours, "hrs"))
    } else {
      failed_posts = failed_posts + 1
      print(paste("✗ Failed:", entry$name, "|", entry$task_name, "| Status:", response$status_code))
    }
    
    # Rate limiting - Wrike allows 100 requests per minute
    Sys.sleep(0.7)  # Wait 0.7 seconds between requests
    
  }, error = function(e) {
    failed_posts = failed_posts + 1
    print(paste("✗ Error posting:", entry$name, "|", e$message))
  })
}

# -------------------------------------------------------------------------
# Results Summary
# -------------------------------------------------------------------------

print("=== MIGRATION RESULTS ===")
print(paste("Successful uploads:", successful_posts))
print(paste("Failed uploads:", failed_posts))
print(paste("Total entries processed:", nrow(time_for_wrike)))

if(failed_posts == 0) {
  print("🎉 SUCCESS! All time entries uploaded to Wrike")
  print("Check your Wrike 'QBT-Wrike Integration Testing' project to see the time entries")
} else {
  print("⚠️  Some uploads failed - check error messages above")
  print("Note: Status 200 responses may actually be successful - check your Wrike project")
}

# Export summary for reference
write.csv(time_for_wrike, paste0("wrike_upload_", Sys.Date(), ".csv"), row.names = FALSE)
print(paste("Summary exported to: wrike_upload_", Sys.Date(), ".csv"))