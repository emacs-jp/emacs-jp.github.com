## Dockerfile

FROM ruby:3.3

COPY Gemfile /

RUN bundle install
