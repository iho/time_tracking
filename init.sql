-- create the database 
-- CREATE DATABASE time_tracking;

-- uuid extension for PostgreSQL
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Enable the pgcrypto extension for UUID generation
CREATE EXTENSION IF NOT EXISTS pgcrypto;    

-- switch to the time_tracking database
\c time_tracking;
-- Create the necessary tables for the time tracking system


-- Users table: Stores employee information
CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Cards table: Maps NFC cards to users
CREATE TABLE cards (
    card_uid VARCHAR(50) PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    assigned_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Work schedules table: Stores regular work schedules for each user
CREATE TABLE work_schedules (
    user_id INTEGER PRIMARY KEY REFERENCES users(user_id) ON DELETE CASCADE,
    start_time TIME NOT NULL, -- Regular start time (e.g., '09:00')
    end_time TIME NOT NULL, -- Regular end time (e.g., '18:00')
    days INTEGER[] NOT NULL, -- Array of days (1=Monday, ..., 7=Sunday)
    free_schedule BOOLEAN NOT NULL DEFAULT FALSE, -- True if no fixed hours
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Exclusions table: Stores exceptions to the regular schedule
CREATE TABLE exclusions (
    exclusion_id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    type_exclusion VARCHAR(20) NOT NULL, -- 'vacation', 'day_off', 'late_arrival', 'early_departure'
    start_datetime TIMESTAMP NOT NULL, -- Start of exclusion period
    end_datetime TIMESTAMP NOT NULL, -- End of exclusion period
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CHECK (end_datetime > start_datetime),
    CHECK (type_exclusion IN ('vacation', 'day_off', 'late_arrival', 'early_departure'))
);

-- Work history table: Stores card touch events (clock-in/clock-out)
CREATE TABLE work_history (
    history_id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    card_uid VARCHAR(50) NOT NULL REFERENCES cards(card_uid) ON DELETE CASCADE,
    touch_time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    event_type VARCHAR(10) NOT NULL, -- 'in' or 'out'
    CHECK (event_type IN ('in', 'out'))
);

-- Indexes for performance
CREATE INDEX idx_work_history_user_id ON work_history(user_id);
CREATE INDEX idx_work_history_touch_time ON work_history(touch_time);
CREATE INDEX idx_exclusions_user_id ON exclusions(user_id);
CREATE INDEX idx_exclusions_start_datetime ON exclusions(start_datetime);
CREATE INDEX idx_cards_user_id ON cards(user_id);