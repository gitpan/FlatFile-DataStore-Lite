#---------------------------------------------------------------------
  package FlatFile::DataStore::Lite::Toc;
#---------------------------------------------------------------------

=head1 NAME

FlatFile::DataStore::Lite::Toc - Perl module that implements a
lightweight flat file data store TOC (table of contents) class.

=head1 SYNOPSYS

 use FlatFile::DataStore::Lite::Toc;
 my $toc;

 $toc = FlatFile::DataStore::Lite::Toc->new( { int => 10,
     datastore => $datastore_obj } );

 # or

 $toc = FlatFile::DataStore::Lite::Toc->new( { num => "A",
     datastore => $datastore_obj } );

=head1 DESCRIPTION

FlatFile::DataStore::Lite::Toc is a Perl module that implements a
lightweight flat file data store TOC (table of contents) class.

This module is used by FlatFile::DataStore::Lite.  You will likely
never call any of it's methods yourself.

=head1 VERSION

FlatFile::DataStore::Lite::Toc version 0.01

=cut

our $VERSION = '0.01';

use 5.008003;
use strict;
use warnings;

use File::Path;
use Carp;
use Math::Int2Base qw( base_chars int2base base2int );

my %Attrs = qw(
    datastore 1
    datafnum  1
    keyfnum   1
    tocfnum   1
    numrecs   1
    keynum    1
    );

#---------------------------------------------------------------------

=head1 CLASS METHODS

=head2 FlatFile::DataStore::Lite::Toc->new( $parms )

Constructs a new FlatFile::DataStore::Lite::Toc object.

The parm C<$parms> is a hash reference containing key/value pairs to
populate the record string.  Three keys are recognized:

 - datastore, data store object (required) and one of:
 - int, data file number as integer, will load object from tocfile
   or
 - num, data file number as number in number base, will load from tocfile

An C<int> or C<num> of 0 will load the first (totals) line from tocfile

=cut

sub new {
    my( $class, $parms ) = @_;

    my $self = bless {}, $class;

    $self->init( $parms ) if $parms;
    return $self;
}


#---------------------------------------------------------------------
# init(), called by new() to parse the parms

sub init {
    my( $self, $parms ) = @_;

    my $ds = $parms->{'datastore'} || croak "Missing datastore";
    $self->datastore( $ds );

    my $datafint;
    if(    defined( my $int = $parms->{'int'} ) ) {
        $datafint = $int; }
    elsif( defined( my $num = $parms->{'num'} ) ) {
        $datafint = base2int $num, $ds->fnumbase; }
    else {
        croak qq/Missing parms 'int' or 'num'./; }

    my $string = $self->read_toc( $datafint );

    unless( $string ) {
        $self->datafnum( $datafint );
        $self->tocfnum(  $self->toc_getfnum( $datafint ) );
        $self->keynum(   $datafint == 0? -1: 0 );
        $self->keyfnum(  0 );
        $self->numrecs(  0 );
        return $self;
    }

    my $fnumbase  = $ds->fnumbase;
    my $keybase   = $ds->keybase;

    my $recsep = $ds->recsep;
    $string =~ s/$recsep$//;  # chompish
    $self->string( $string );

    my @fields = split " ", $string;
    my $i = 0;
    $self->$_( base2int $fields[ $i++ ], $fnumbase )
        for qw( datafnum keyfnum tocfnum );
    $self->$_( base2int $fields[ $i++ ], $keybase )
        for qw( numrecs keynum );

    return $self;
}

#---------------------------------------------------------------------

=head1 OBJECT METHODS

=cut

#---------------------------------------------------------------------
sub to_string {
    my( $self ) = @_;

    my $ds = $self->datastore;

    my $fnumbase  = $ds->fnumbase;
    my $fnumlen   = $ds->fnumlen;
    my $keybase   = $ds->keybase;
    my $keylen    = $ds->keylen;

    my @fields;
    push @fields, int2base $self->$_(), $fnumbase, $fnumlen
        for qw( datafnum keyfnum tocfnum );
    push @fields, int2base $self->$_(), $keybase, $keylen
        for qw( numrecs keynum );

    return join( " " => @fields ) . $ds->recsep;
}

#---------------------------------------------------------------------
sub read_toc {
    my( $self, $fint ) = @_;

    my $ds = $self->datastore;

    my $tocfile = $self->tocfile( $fint );
    return unless -e $tocfile;

    my $tocfh  = $ds->locked_for_read( $tocfile );
    my $toclen = $ds->toclen;

    my $seekpos;
    if( my $tocmax = $ds->tocmax ) {
        my $skip = int( $fint / $tocmax );
        $seekpos = $toclen * ( $fint - ( $skip * $tocmax ) ); }
    else {
        $seekpos = $toclen * $fint; }

    return $ds->read_bytes( $tocfh, $seekpos, $toclen );
}

#---------------------------------------------------------------------
sub write_toc {
    my( $self, $fint ) = @_;

    my $ds = $self->datastore;

    my $tocfh   = $ds->locked_for_write( $self->tocfile( $fint ) );
    my $toclen  = $ds->toclen;

    my $seekpos;
    if( my $tocmax = $ds->tocmax ) {
        my $skip = int( $fint / $tocmax );
        $seekpos = $toclen * ( $fint - ( $skip * $tocmax ) ); }
    else {
        $seekpos = $toclen * $fint; }

    return $ds->write_bytes( $tocfh, $seekpos, $self->to_string );
}

#---------------------------------------------------------------------
# toc_getfnum(), called by tocfile() and init()
sub toc_getfnum {
    my( $self, $fint ) = @_;

    my $ds = $self->datastore;

    # get toc file number based on tocmax and fint
    my $tocfint;

    my $tocmax = $ds->tocmax;
    if( $tocmax ) { $tocfint = int( $fint / $tocmax ) + 1 }
    else          { $tocfint = 1                          }

    my $fnumlen  = $ds->fnumlen;
    my $fnumbase = $ds->fnumbase;
    my $tocfnum = int2base $tocfint, $fnumbase, $fnumlen;
    croak qq/Database exceeds configured size (tocfnum: "$tocfnum" too long)/
        if length $tocfnum > $fnumlen;
    return( $tocfint, $tocfnum ) if wantarray;
    return $tocfint;
}

#---------------------------------------------------------------------
sub tocfile {
    my( $self, $fint ) = @_;

    my $ds = $self->datastore;

    my $name = $ds->name;

    my( $tocfint, $tocfnum ) = $self->toc_getfnum( $fint );
    my $tocfile = $name . ( $ds->tocmax? ".$tocfnum": "") . ".toc";

    # get toc path based on dirlev, dirmax, and toc file number
    if( my $dirlev = $ds->dirlev ) {
        my $fnumlen  = $ds->fnumlen;
        my $fnumbase = $ds->fnumbase;
        my $dirmax   = $ds->dirmax;
        my $path     = "";
        my $this     = $tocfint;
        for( 1 .. $dirlev ) {
            my $dirint = $dirmax? (int( ( $this - 1 ) / $dirmax ) + 1): 1;
            my $dirnum = int2base $dirint, $fnumbase, $fnumlen;
            $path = $path? "$dirnum/$path": $dirnum;
            $this = $dirint;
        }
        $path = "$name/toc$path";
        mkpath( $path ) unless -d $path;
        $tocfile = "$path/$tocfile";
    }

    return $ds->dir . "/$tocfile";
}

#---------------------------------------------------------------------

=head1 OBJECT METHODS: Accessors

The following read/write methods set and return their respective
attribute values if C<$value> is given.  Otherwise, they just return
the value.

 $record->datastore( [$value] )
 $record->string(    [$value] )

The following methods expect an integer parm and return an integer
value (even though these are stored in the tocfile as numbers in their
respective bases).

 $record->datafnum( [$value] )
 $record->keyfnum(  [$value] )
 $record->tocfnum(  [$value] )
 $record->numrecs(  [$value] )
 $record->keynum(   [$value] )

=cut

sub datastore {for($_[0]->{datastore} ){$_=$_[1]if@_>1;return$_}}
sub string    {for($_[0]->{string}    ){$_=$_[1]if@_>1;return$_}}

sub datafnum  {for($_[0]->{datafnum}  ){$_=$_[1]if@_>1;return$_}}
sub keyfnum   {for($_[0]->{keyfnum}   ){$_=$_[1]if@_>1;return$_}}
sub tocfnum   {for($_[0]->{tocfnum}   ){$_=$_[1]if@_>1;return$_}}
sub numrecs   {for($_[0]->{numrecs}   ){$_=$_[1]if@_>1;return$_}}
sub keynum    {for($_[0]->{keynum}    ){$_=$_[1]if@_>1;return$_}}

__END__

=head1 AUTHOR

Brad Baxter, E<lt>bbaxter@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Brad Baxter

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.

=cut

