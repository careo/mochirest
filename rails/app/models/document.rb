class Document < ActiveResource::Base
  self.site = "http://localhost:8000/"
  self.format = :json
  self.primary_key = "_id"
end
