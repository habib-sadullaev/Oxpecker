﻿module ContactApp.templates.show

open ContactApp.Models
open Oxpecker.ViewEngine

let html (contact: Contact) =
    __() {
        h1() { $"{contact.First} {contact.Last}" }

        div() {
            div() { $"Phone: {contact.Phone}" }
            div() { $"Email: {contact.Email}" }
        }

        p() {
            a(href= $"/contacts/{contact.Id}/edit") { "Edit" }
            a(href="/contacts") { "Back" }
        }
    }
    |> layout.html
