#!/usr/bin/env perl

use Cwd;

open SOURCES, "<sources" or die $!;

while (<SOURCES>) {
    next if /^\s*\#|^\s*$/;
    chomp;
    my ($dir, $type, @arguments) = split(/\|/, $_);
    print "$dir:$type:";

    if ($#ARGV < 0 or grep(/$dir/, @ARGV)) {
        print "updating\n";
        &{$type}($dir, @arguments);
    } else { print "skipped\n"; }
}

sub cvs ($$$$) {
    my $dir = shift, $root = shift, $module = shift, $tag = shift;

    if ($tag == "") {
        $tagArg = "";
    } else {
        $tagArg = "-r$tag";
    }

    if (-e $dir) {
        my $old_dir = getcwd;
        # Maybe update

    } else {
        my $cmd = "cvs -d$root co $tagArg $module";
        print "$cmd\n";
        open CMD, "$cmd|";
        print while (<CMD>);
    }
}

sub svn ($$) {
    my $dir = shift, $repo = shift;

    if (-e $dir) {
        my $old_dir = getcwd;
        # Maybe update

    } else {
        my $cmd = "svn co $repo $dir";
        open CMD, "$cmd|";
        print while (<CMD>);
    }
}

sub curl($$) {
    my $file = shift, $url = shift;
    return if (-e $file);
    system("mkdir -p `dirname $file`");
    open CURL, "curl -o '$file' '$url'|";
    print while (<CURL>);
}

sub tgz ($$$) {
    my $dir = shift, $url = shift, $res = shift;
    return if (-e $dir);
    print "Downloading from: $url\n";
    open CURL, "curl '$url' | tar xvzf -|";
    print while (<CURL>);
    `mv "$res" "$dir"` unless ($res eq "");
}

sub zip ($$$) {
    my $dir = shift, $url = shift, $res = shift;
    return if (-e $dir);
    my $tmpf = "tmp.zip";
    print "Downloading from: $url\n";
    open CURL, "curl '$url' -o $tmpf|";
    print while (<CURL>);
    open UNZIP, "unzip $tmpf|";
    print while (<UNZIP>);
    `rm $tmpf`;
    `mv "$res" "$dir"` unless ($res eq "");
}

sub bzr_tgz ($$$$) {
    my $dir = shift, $bzr_url = shift, $tgz_url = shift, $res = shift;
    return if (-e $dir);

    open BZR, "bzr get $bzr_url $dir |";
    print while (<BZR>);
    close BZR;
    unless ($? == 0) {
        system("rm -rf $dir");
        tgz($dir, $tgz_url, $res);
    }
}

sub git ($$$$) {
    my $dir = shift, $git_url = shift;
    return if (-e $dir); # Update?

    open GIT, "git clone $git_url $dir |";
    print while (<GIT>);
    close GIT;
}

print "\n";
print "Done!\n";
