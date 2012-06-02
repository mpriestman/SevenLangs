#!/usr/bin/ruby

module ActsAsCsv
  class CsvRow
    def initialize(data)
      @data = data
    end
    
    def method_missing name, *args
      @data[name.to_s]
    end
  end
  
  def self.included(base)
    base.extend ClassMethods
  end
  
  module ClassMethods
    def acts_as_csv
      include InstanceMethods
    end
  end
  
  module InstanceMethods
    def read
      @csv_contents = []
      file = File.new(self.class.to_s.downcase + '.txt')
      @headers = file.gets.chomp.split(', ')

      file.each do |row|
        @csv_contents << CsvRow.new(Hash[@headers.zip(row.chomp.split(', '))])
      end
    end
    
    def each
      @csv_contents.each { |row| yield row }
    end
    
    attr_accessor :headers, :csv_contents
    
    def initialize
      read
    end
  end
end

class RubyCsv
  include ActsAsCsv
  acts_as_csv
end

m = RubyCsv.new
m.each { |row| puts row.Name }