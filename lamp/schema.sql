-- Create Authors table
CREATE TABLE Authors (
    AuthorID INT PRIMARY KEY,
    Name VARCHAR(100),
    Country VARCHAR(50)
);

-- Create Books table
CREATE TABLE Books (
    BookID INT PRIMARY KEY,
    Title VARCHAR(200),
    Genre VARCHAR(50),
    AuthorID INT,
    Price DECIMAL(5,2),
    FOREIGN KEY (AuthorID) REFERENCES Authors(AuthorID)
);

-- Create Sales table
CREATE TABLE Sales (
    SaleID INT PRIMARY KEY,
    BookID INT,
    SaleDate DATE,
    Quantity INT,
    FOREIGN KEY (BookID) REFERENCES Books(BookID)
);

-- Insert sample authors
INSERT INTO Authors (AuthorID, Name, Country) VALUES
(1, 'Haruki Murakami', 'Japan'),
(2, 'J.K. Rowling', 'United Kingdom'),
(3, 'Chimamanda Ngozi Adichie', 'Nigeria');

-- Insert sample books
INSERT INTO Books (BookID, Title, Genre, AuthorID, Price) VALUES
(101, 'Kafka on the Shore', 'Magical Realism', 1, 14.99),
(102, 'Harry Potter and the Sorcerers Stone', 'Fantasy', 2, 19.99),
(103, 'Half of a Yellow Sun', 'Historical Fiction', 3, 13.50);

-- Insert sample sales
INSERT INTO Sales (SaleID, BookID, SaleDate, Quantity) VALUES
(1001, 101, '2025-08-01', 3),
(1002, 102, '2025-08-02', 5),
(1003, 103, '2025-08-03', 2);
