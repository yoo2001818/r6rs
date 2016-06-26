var gulp = require('gulp');
var babel = require('gulp-babel');
var eslint = require('gulp-eslint');
var mocha = require('gulp-mocha');
var revertPath = require('gulp-revert-path');
require('babel-register');

// TODO: Add code coverage tool

gulp.task('lint', function () {
  return gulp.src(['src/**/*.js', 'client-test/**/*.js'])
    .pipe(eslint())
    .pipe(eslint.format())
    .pipe(eslint.failOnError());
});

gulp.task('mocha', function() {
  return gulp.src(['test/**/*.js'], { read: false })
    .pipe(mocha({ reporter: 'spec' }));
});

gulp.task('mochaSimple', function() {
  return gulp.src(['test/**/*.js'], { read: false })
    .pipe(mocha({ reporter: 'min' }));
});

gulp.task('test', ['lint', 'mocha']);

gulp.task('watch', function() {
  return gulp.watch(['src/**/*.js', 'test/**/*.js'], ['mochaSimple']);
});

gulp.task('babel', function() {
  return gulp.src(['src/**/*.*'])
    .pipe(babel())
    .pipe(revertPath())
    .pipe(gulp.dest('lib'));
});

gulp.task('default', ['babel']);
