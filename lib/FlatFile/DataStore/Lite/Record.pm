#---------------------------------------------------------------------
  package FlatFile::DataStore::Lite::Record;
#---------------------------------------------------------------------

=head1 NAME

FlatFile::DataStore::Lite::Record - Perl module that implements a
lightweight flat file data store record class.

=head1 SYNOPSYS

 # first, create a preamble object

 use FlatFile::DataStore::Lite::Preamble;

 my $preamble = FlatFile::DataStore::Lite::Preamble->new( {
     indicator => $indicator,  # single-character crud flag
     keynum    => $keynum,     # record sequence number (integer)
     reclen    => $reclen,     # record length (integer)
     thisfnum  => $fnum,       # file number (in base format)
     thisseek  => $datapos,    # seek position (integer)
     user      => $user_data,  # pre-formatted user-defined data
     } );

 # then create a record object with the preamble contained in it

 use FlatFile::DataStore::Lite::Record;

 my $record = FlatFile::DataStore::Lite::Record->new( {
     preamble => $preamble,                 # i.e., a preamble object
     data     => "This is a test record.",  # actual record data
     } );

=head1 DESCRIPTION

FlatFile::DataStore::Lite::Record is a Perl module that implements a
lightweight flat file data store record class.  This class defines
objects used by FlatFile::DataStore::Lite.  You will likely not ever
call new() yourself, (FlatFile::DataStore::Lite::create() would, e.g.,
do that) but you will likely call the accessors.

=head1 VERSION

FlatFile::DataStore::Lite::Record version 0.01

=cut

our $VERSION = '0.01';

use 5.008003;
use strict;
use warnings;

use Carp;

my %Attrs = qw(
    preamble  1
    data      1
    );

#---------------------------------------------------------------------

=head1 CLASS METHODS

=head2 FlatFile::DataStore::Lite::Record->new( $parms )

Constructs a new FlatFile::DataStore::Lite::Record object.

The parm C<$parms> is a hash reference containing key/value pairs to
populate the record string.  Two keys are recognized:

 - preamble, i.e., a FlatFile::DataStore::Lite::Preamble object
 - data,     the actual record data

The record data is stored as a scalar reference.

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

    # want to store record data as a scalar reference
    for( $parms->{'data'} ) {
        if( defined ) {
            if( ref eq 'SCALAR' ) { $self->data( $_  ) }
            else                  { $self->data( \$_ ) }
        }
        else                      { $self->data( \"" ) }
    }

    if( my $preamble = $parms->{'preamble'} ) {
        $self->preamble( $preamble );
    }
    
    return $self;
}

#---------------------------------------------------------------------

=head1 OBJECT METHODS: ACCESSORS

The following read/write methods set and return their respective
attribute values if C<$value> is given.  Otherwise, they just return
the value.

 $record->data(     [$value] ); # actual record data
 $record->preamble( [$value] ); # FlatFile::DataStore::Lite::Preamble object

=cut

sub data     {for($_[0]->{data}    ){$_=$_[1]if@_>1;return$_}}
sub preamble {for($_[0]->{preamble}){$_=$_[1]if@_>1;return$_}}

=pod

The following read-only methods just return their respective values.
The values all come from the record's contained preamble object.

 $record->user()
 $record->string()
 $record->indicator()
 $record->keynum()
 $record->reclen()
 $record->thisfnum()
 $record->thisseek()

=cut

sub user {for($_[0]->preamble()){defined&&return$_->user()}}

sub string    {$_[0]->preamble()->string()   }
sub indicator {$_[0]->preamble()->indicator()}
sub keynum    {$_[0]->preamble()->keynum()   }
sub reclen    {$_[0]->preamble()->reclen()   }
sub thisfnum  {$_[0]->preamble()->thisfnum() }
sub thisseek  {$_[0]->preamble()->thisseek() }

__END__

=head1 AUTHOR

Brad Baxter, E<lt>bbaxter@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Brad Baxter

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.

=cut

