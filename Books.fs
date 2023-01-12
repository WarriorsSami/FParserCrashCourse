module FParserCrashCourse.Books

type Books = {
    Title: string
    Author: string
    Category: string
    PublishYear: int
    Rating: float
}

let getAll() = [
    {
        Title = "The Hobbit"
        Author = "J.R.R. Tolkien"
        Category = "Fantasy"
        PublishYear = 1937
        Rating = 4.25
    }
    {
        Title = "The Lord of the Rings"
        Author = "J.R.R. Tolkien"
        Category = "Fantasy"
        PublishYear = 1954
        Rating = 4.49
    }
    {
        Title = "The Silmarillion"
        Author = "J.R.R. Tolkien"
        Category = "Fantasy"
        PublishYear = 1977
        Rating = 4.14
    }
    {
        Title = "Warriors Don't Cry"
        Author = "Melba Pattillo Beals"
        Category = "Non-Fiction"
        PublishYear = 1994
        Rating = 4.11
    }
    {
        Title = "The Color Purple"
        Author = "Alice Walker"
        Category = "Fiction"
        PublishYear = 1982
        Rating = 4.06
    }
    {
        Title = "The Handmaid's Tale"
        Author = "Margaret Atwood"
        Category = "Fiction"
        PublishYear = 1985
        Rating = 4.09
    }
]